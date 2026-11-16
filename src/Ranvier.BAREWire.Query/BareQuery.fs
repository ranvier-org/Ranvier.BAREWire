namespace Ranvier.BAREWire.Query

open System
open Ranvier
open Ranvier.Query
open Ranvier.BAREWire

/// <summary>Ranvier.Query families and mutations whose remote calls are BAREWire asks.</summary>
[<AutoOpen>]
module BareQueryExtensions =

    type QueryClient with

        /// <summary>
        /// Defines a query family loaded by a BAREWire ask: the key is the Ask's payload and the data is the
        /// Reply's. Performs no IO; keys use the default equality comparer.
        /// </summary>
        /// <remarks>A failed ask is the query's <c>Error</c>, a <see cref="T:Ranvier.BAREWire.BareWireException"/> for a frame or payload that does not decode.</remarks>
        member client.DefineBare(bare: BareClient, request: BareCodec<'Key>, reply: BareCodec<'T>) : QueryFamily<'Key, 'T> =
            client.Define (bare.Asker (request, reply))

        /// <summary>
        /// Defines a query family over a protocol whose asks and replies are each one union: <c>toRequest</c> builds
        /// the ask from the key, and <c>ofReply</c> takes the awaited case from the reply, raising for any other.
        /// </summary>
        member client.DefineBare
            (bare: BareClient, request: BareCodec<'Request>, reply: BareCodec<'Reply>, toRequest: 'Key -> 'Request, ofReply: 'Reply -> 'T)
            : QueryFamily<'Key, 'T> =
            client.Define (bare.Asker (request, reply, toRequest, ofReply))

        /// <summary>
        /// Queues one remote write sent as a BAREWire ask, then publishes the edits <c>reconcile</c> describes from
        /// its Reply before the next write starts.
        /// </summary>
        /// <returns>A task with the mutation outcome, as <c>QueryClient.Mutate</c> returns it.</returns>
        member client.MutateBare
            (bare: BareClient, request: BareCodec<'Input>, reply: BareCodec<'Result>, input: 'Input, reconcile: 'Result -> QueryEdit list)
            =
            client.Mutate (input, bare.Asker (request, reply), reconcile)

    type BareDuplex with

        /// <summary>
        /// Calls <c>handler</c> on the graph's thread with the decoded payload of each Event frame received, or with
        /// the reason its payload did not decode, until the result is disposed.
        /// </summary>
        /// <remarks>
        /// The handler may commit query edits: a server event that names changed data becomes
        /// <c>client.Commit [ family.Invalidate key ]</c>.
        /// </remarks>
        member duplex.OnEvent(graph: Graph, codec: BareCodec<'Event>, handler: Result<'Event, BareError> -> unit) : IDisposable =
            // Dispatch runs the work inline on the graph's thread and through the graph's dispatcher from any other.
            duplex.OnEvent (codec, fun received -> graph.Dispatch (Action (fun () -> handler received)))
