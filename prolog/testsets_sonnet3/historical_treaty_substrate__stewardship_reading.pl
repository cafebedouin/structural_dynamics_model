% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__stewardship_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__stewardship_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: historical_treaty_substrate__stewardship_reading
 *   human_readable: Historical Treaty Substrate — Shared Stewardship Reading
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   Historical treaties negotiated between Indigenous nations and colonial or
 *   settler state authorities are subject to multiple structurally distinct
 *   interpretive readings of a single textual and oral-historical substrate.
 *   The stewardship reading holds that these agreements never contemplated a
 *   transfer of underlying sovereignty or title, but instead established
 *   durable relationships of mutual obligation — shared stewardship of
 *   territory and resources, consultation duties, and coexistence norms.
 *   Under this reading, the settler state's current practice of unilateral
 *   resource permitting and land administration constitutes an ongoing breach
 *   of the treaty's actual terms, not a lawful exercise of ceded authority.
 *   The constraint as it operates today extracts substantially from
 *   Indigenous treaty nations relative to what the stewardship reading holds
 *   they are owed, even as courts and political rhetoric increasingly cite
 *   stewardship language.
 *
 * KEY AGENTS:
 *   - indigenous_treaty_nations: primary rights-holder and target of ongoing extraction under the unresolved reading
 *   - settler_state_governments: administers the substrate, selectively invokes stewardship language, bears enforcement burden of current allocation
 *   - settler_state_resource_sector: concentrated beneficiary of unilateral resource permitting
 *   - future_generations_of_treaty_nations: bear deferred costs of unresolved jurisdiction
 *   - courts_and_treaty_commissions: analytical/adjudicative seat increasingly sympathetic to stewardship framing but with limited implementation power
 *   - non_treaty_resource_dependent_settler_communities: excluded from treaty governance despite material stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, 0.71).
domain_priors:suppression_score(historical_treaty_substrate__stewardship_reading, 0.68).
domain_priors:theater_ratio(historical_treaty_substrate__stewardship_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(historical_treaty_substrate__stewardship_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__stewardship_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__stewardship_reading, "Historical Treaty Substrate — Shared Stewardship Reading").
narrative_ontology:topic_domain(historical_treaty_substrate__stewardship_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__stewardship_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__stewardship_reading, '56c78be4-0286-4d83-8587-46a06bb9a6a0').
narrative_ontology:cs_kernel_codification('56c78be4-0286-4d83-8587-46a06bb9a6a0', distributed).
narrative_ontology:cs_authority_grounding('56c78be4-0286-4d83-8587-46a06bb9a6a0', distributed).
narrative_ontology:cs_reading_relation('56c78be4-0286-4d83-8587-46a06bb9a6a0', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('56c78be4-0286-4d83-8587-46a06bb9a6a0', historical_treaty_substrate__nation_to_nation_reading, coexists_with).
narrative_ontology:cs_axiom('56c78be4-0286-4d83-8587-46a06bb9a6a0', foundational, sovereignty_was_never_ceded).
narrative_ontology:cs_axiom_status(sovereignty_was_never_ceded, holdable).
narrative_ontology:cs_axiom_grounding('56c78be4-0286-4d83-8587-46a06bb9a6a0', sovereignty_was_never_ceded, empirically_contingent).
narrative_ontology:cs_axiom('56c78be4-0286-4d83-8587-46a06bb9a6a0', foundational, territorial_relationship_generates_ongoing_mutual_obligation).
narrative_ontology:cs_axiom_status(territorial_relationship_generates_ongoing_mutual_obligation, holdable).
narrative_ontology:cs_axiom_grounding('56c78be4-0286-4d83-8587-46a06bb9a6a0', territorial_relationship_generates_ongoing_mutual_obligation, conventional).
narrative_ontology:cs_reference_frame('56c78be4-0286-4d83-8587-46a06bb9a6a0', pre_contact_coexistence_norms).
narrative_ontology:cs_drift_state('56c78be4-0286-4d83-8587-46a06bb9a6a0', contemporary_reconciliation_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('56c78be4-0286-4d83-8587-46a06bb9a6a0', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_nations).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, shared_stewardship_doctrine).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__stewardship_reading, treaty_as_living_relationship_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the original understanding that the treaty established an ongoing relationship of mutual obligation over shared land, not a transfer of underlying title. Under the stewardship reading they remain rightful co-jurisdiction holders over territory and resources. In practice they receive fragmentary consultation and revenue-sharing arrangements while the settler state continues to administer land-use, resource extraction, and regulatory authority unilaterally in most operational domains. Exit from the treaty relationship is not available without abandoning the very claim of the relationship's proper meaning — the community's legal and cultural identity is bound to the treaty's correct interpretation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__stewardship_reading, indigenous_treaty_nations, payer).

% Administer land title registries, resource permitting, and jurisdictional boundaries as though the treaty substantially settled sovereignty questions in the state's favor, while formally acknowledging (in some jurisdictions) an ongoing 'nation-to-nation' or 'stewardship' relationship in political rhetoric and some court rulings. Can selectively invoke stewardship language for legitimacy purposes without fully restructuring resource governance. Bears the enforcement burden of maintaining current land tenure and resource allocation systems against stewardship-based claims.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_governments, agenda_setter,
    institutional, generational, arbitrage, national).

% Extracts timber, minerals, hydrocarbons, and hydroelectric potential from treaty territories under permits issued by settler state governments, often with minimal or purely consultative Indigenous involvement. Benefits directly from a settled-title interpretation of the treaty substrate; a genuine shift to joint management would impose new consent requirements, revenue-sharing obligations, and veto points that raise costs and slow project approval.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector, beneficiary,
    powerful, biographical, mobile, national).

% Inherit whatever resource base and jurisdictional authority remains after current extraction continues under the unresolved reading. If the stewardship interpretation is not operationalized now, they inherit a diminished land and resource base along with an unresolved legal claim that grows harder to vindicate as facts on the ground (development, resource depletion, demographic change) accumulate.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, future_generations_of_treaty_nations, payer,
    powerless, civilizational, trapped, regional).

% Adjudicate treaty rights litigation and land claims processes, weighing historical evidence including oral history, treaty text, and negotiation records. Increasingly cite stewardship and relational framings in landmark rulings, but implementation of these rulings into administrative practice lags substantially, leaving the doctrine's practical force contested.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, courts_and_treaty_commissions, observer,
    institutional, generational, analytical, national).

% Local settler communities whose employment and municipal tax base depend on resource extraction from treaty territories are rarely party to treaty negotiations or stewardship governance discussions, yet would be materially affected by a shift to joint Indigenous-state resource management. Their interests are mediated entirely through state and industry actors, not represented directly.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__stewardship_reading, non_treaty_resource_dependent_settler_communities, excluded,
    moderate, biographical, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__stewardship_reading, settler_state_resource_sector).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__stewardship_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an ongoing framework for two or more polities to share a territory and its resources without either extinguishing the other's political existence — mutual obligations of consultation, resource-sharing, and coexistence substituting for permanent property transfer or conquest.
% TRANSFER_FUNCTION: Under the stewardship reading, resource revenue, land-use authority, and regulatory control should flow bidirectionally through joint governance mechanisms; in the actually operating arrangement, they flow predominantly from treaty territories to the settler state and its licensed resource sector, with Indigenous nations receiving partial consultation rights and negotiated compensation rather than co-equal management authority.
% ABSENT_VOICES: Non-treaty settler communities dependent on current resource arrangements are not present in treaty interpretation processes despite being materially affected by any shift toward joint stewardship. Future generations of treaty nations, whose resource inheritance is being determined now, have no direct voice in present negotiations or litigation.
% DISAPPEARANCE_RATIONALE: If the treaty substrate were treated as void rather than merely re-read, land title, resource permitting, and jurisdictional authority across vast territories would become legally unmoored — every extraction permit, municipal boundary, and land registry entry resting on the settler state's assumed authority would face renewed challenge, and Indigenous nations' claims to co-jurisdiction would have no textual anchor at all. The treaty's existence, regardless of reading, structures the entire architecture of who currently administers what.
% FOUNDING_PROBLEM: Early contact-era polities needed a mechanism for coexistence on shared territory that avoided continuous warfare — a way to establish mutual recognition, resource-sharing norms, and conduct rules between distinct nations occupying overlapping land.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous oral historical accounts, preserved in treaty commission testimony and increasingly credited in court rulings (e.g., judicial recognition of oral history as valid evidence of treaty intent), corroborate the stewardship/relational reading as the original understanding from the Indigenous negotiating side. Independent historians and linguists analyzing negotiation records in original languages have also found textual and contextual support for a non-cession reading — this corroboration comes from scholars and jurists outside the Indigenous nations themselves, not merely from the beneficiary parties asserting their own claim.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__stewardship_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__stewardship_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__stewardship_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__stewardship_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__stewardship_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__stewardship_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__stewardship_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__stewardship_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.71 at interval end, having declined from 0.88 at treaty founding) because, under the stewardship reading's own terms, the settler state and its licensed resource sector continue to capture the overwhelming share of territorial resource value and administrative authority that the treaty's actual terms — mutual obligation, shared jurisdiction, ongoing consent — never ceded. Suppression is high (0.68, declining from 0.92) reflecting the historical and continuing coercive apparatus (land registries, permitting regimes, policing of resource access, historical prohibition on Indigenous legal assertion) that has enforced the extinguishment-adjacent operational practice regardless of the treaty's stewardship content. Theater ratio rises over the interval (0.10 to 0.42) reflecting a real historical pattern: as courts and governments increasingly adopt stewardship and reconciliation rhetoric, an increasing share of state activity is performative acknowledgment (land acknowledgments, consultation processes with no binding force, symbolic co-management boards with narrow mandates) rather than the substantive jurisdictional and resource-sharing restructuring the stewardship reading actually requires. This is a rope-shaped coordination function (coexistence on shared territory without perpetual conflict) captured by asymmetric extraction machinery that requires active enforcement to sustain — the tangled rope structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous treaty nations are declared BOTH beneficiary (they hold the rightful stewardship claim the reading vindicates) and payer (the operational arrangement extracts from them relative to that claim) — this dual role is intentional and reflects the gap between doctrinal entitlement and administrative practice. Settler state governments sit as agenda_setter: institutional power, arbitrage-grade exit (can invoke stewardship rhetoric selectively without full restructuring), directionality toward the low-extraction end because they administer rather than bear the constraint. The settler state resource sector is a concentrated beneficiary with mobile exit (can relocate capital) and powerful position — directionality strongly toward the beneficiary end. Future generations of treaty nations are powerless and trapped (civilizational time horizon, no present voice, inheriting whatever remains) — directionality pushed toward the extreme target end even though they are not yet born into the current dispute.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coexistence on shared territory without perpetual conflict — remains live (contested status): Indigenous nations and increasingly courts affirm the underlying relational need is unresolved, not obsolete, while settler state governments' practice suggests they treat the sovereignty question as functionally settled. This divergence is precisely what the founding_problem_status field is designed to expose: a status of 'contested' paired with a disappearance_verdict of 'world_rearranges' signals that the treaty substrate has NOT become mere zombie ritual (which would show status=dead + verdict=world_rearranges as a capture flag) — instead it shows a live, unresolved doctrinal fight actively structuring present resource governance, which is the tangled_rope condition rather than either a pure rope (fully resolved coordination) or a pure snare (no coordination function at all).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stewardship_vs_extinguishment_evidentiary_weight,
    'Does the weight of historical negotiation evidence (oral history, contemporaneous translation records, subsequent conduct of the parties) favor the stewardship reading over the extinguishment reading as the treaty''s original and controlling meaning?',
    'Comprehensive comparative linguistic and historical analysis of original-language negotiation records, cross-referenced against subsequent conduct of both parties in the first decades after signing, adjudicated through treaty commission and court processes with standing to weigh oral history as primary evidence.',
    'If the stewardship reading is evidentially dominant, the settler state''s operational practice constitutes a long-running breach with a strong remedial claim; if the extinguishment reading is evidentially dominant, the current arrangement is closer to a completed (if historically coercive) transaction and the appropriate remedy shifts from restructuring current governance to compensating for the historical coercion itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stewardship_vs_extinguishment_evidentiary_weight, empirical, 'Whether the historical evidentiary record favors this reading over the extinguishment reading.').

omega_variable(
    stewardship_operationalization_feasibility,
    'Is joint territorial and resource stewardship as this reading demands institutionally and economically feasible within the existing settler state legal and administrative architecture, or does it require constitutional-level restructuring?',
    'Comparative case study of jurisdictions that have implemented binding co-management regimes (rather than consultative-only structures) and measurement of resulting resource governance outcomes, fiscal flows, and dispute frequency.',
    'If genuinely operationalizable within existing structures, the persistent gap between doctrine and practice is better explained by settler state unwillingness (supporting a snare-leaning reading of current administrative practice); if it requires deep restructuring, some of the measured extraction reflects genuine transition friction rather than pure extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stewardship_operationalization_feasibility, conceptual, 'Whether the stewardship reading''s implied governance structure is administratively feasible short of major state restructuring.').

omega_variable(
    reading_selection_committer_framing,
    'Is the stewardship reading the historically dominant Indigenous understanding across all treaty relationships in this substrate, or does it reflect a subset of nations/treaties while others align more closely with nation-to-nation or even partial-cession framings?',
    'Treaty-by-treaty historical and linguistic analysis rather than a single substrate-wide characterization; the kernel may itself decompose further by treaty family or negotiating nation.',
    'If treaty-specific variation is substantial, this story''s ε and stakeholder structure may only validly apply to a subset of the historical treaty substrate, and further decomposition into treaty-family-specific constraints would be warranted under the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_committer_framing, conceptual, 'Whether one stewardship reading validly covers the entire treaty substrate or requires further per-treaty decomposition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__stewardship_reading, 0, 150).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t0, historical_treaty_substrate__stewardship_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(hist_tr_t25, historical_treaty_substrate__stewardship_reading, theater_ratio, 25, 0.15).
narrative_ontology:measurement(hist_tr_t50, historical_treaty_substrate__stewardship_reading, theater_ratio, 50, 0.22).
narrative_ontology:measurement(hist_tr_t75, historical_treaty_substrate__stewardship_reading, theater_ratio, 75, 0.3).
narrative_ontology:measurement(hist_tr_t100, historical_treaty_substrate__stewardship_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement(hist_tr_t125, historical_treaty_substrate__stewardship_reading, theater_ratio, 125, 0.4).
narrative_ontology:measurement(hist_tr_t150, historical_treaty_substrate__stewardship_reading, theater_ratio, 150, 0.42).

% Extraction over time
narrative_ontology:measurement(hist_be_t0, historical_treaty_substrate__stewardship_reading, base_extractiveness, 0, 0.88).
narrative_ontology:measurement(hist_be_t25, historical_treaty_substrate__stewardship_reading, base_extractiveness, 25, 0.86).
narrative_ontology:measurement(hist_be_t50, historical_treaty_substrate__stewardship_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement(hist_be_t75, historical_treaty_substrate__stewardship_reading, base_extractiveness, 75, 0.79).
narrative_ontology:measurement(hist_be_t100, historical_treaty_substrate__stewardship_reading, base_extractiveness, 100, 0.76).
narrative_ontology:measurement(hist_be_t125, historical_treaty_substrate__stewardship_reading, base_extractiveness, 125, 0.73).
narrative_ontology:measurement(hist_be_t150, historical_treaty_substrate__stewardship_reading, base_extractiveness, 150, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t0, historical_treaty_substrate__stewardship_reading, suppression_requirement, 0, 0.92).
narrative_ontology:measurement(hist_su_t25, historical_treaty_substrate__stewardship_reading, suppression_requirement, 25, 0.88).
narrative_ontology:measurement(hist_su_t50, historical_treaty_substrate__stewardship_reading, suppression_requirement, 50, 0.82).
narrative_ontology:measurement(hist_su_t75, historical_treaty_substrate__stewardship_reading, suppression_requirement, 75, 0.78).
narrative_ontology:measurement(hist_su_t100, historical_treaty_substrate__stewardship_reading, suppression_requirement, 100, 0.74).
narrative_ontology:measurement(hist_su_t125, historical_treaty_substrate__stewardship_reading, suppression_requirement, 125, 0.7).
narrative_ontology:measurement(hist_su_t150, historical_treaty_substrate__stewardship_reading, suppression_requirement, 150, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__stewardship_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__stewardship_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__stewardship_reading, historical_treaty_substrate__nation_to_nation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the historical_treaty_substrate kernel, each authored as a separate constraint with its own ε per the ε-invariance principle: extinguishment_reading (property-transaction framing, likely highest ε against Indigenous nations, cession is complete and current arrangement is the settled endpoint), nation_to_nation_reading (sovereign-equals framing emphasizing ongoing bilateral consent and modern treaty law), and this stewardship_reading (relational/coexistence framing, no cession, joint management obligation). All three describe the same underlying textual/oral-historical substrate but instantiate structurally distinct constraints with different beneficiary/victim sets and different extraction profiles under their own terms. Network edges reflect that court rulings and political developments favoring this reading create downstream legitimacy and resource pressure on the other two readings' operative force.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
