% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__land_promise_constraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__land_promise_constraint, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: abrahamic_covenant__land_promise_constraint
 *   human_readable: Covenant Territorial Grant as Legitimacy Structure (Land Promise Reading)
 *   domain: religious/comparative_theology/institutional_authority
 *
 * SUMMARY:
 *   This story instantiates the land_promise_constraint reading of the
 *   abrahamic_covenant kernel: the claim that the covenant includes a
 *   territorial grant of Canaan that is operative today. The standing
 *   arrangement under contest is the modern operation of that reading —
 *   ancestral title invoked to ground and maintain territorial control,
 *   administered by state machinery, and borne by displaced and occupied
 *   populations. The ε referent is that standing arrangement as this
 *   authoring seat sees it, never the rights-respecting alternative the
 *   critic would prefer. Scoping decision: this story bounds the arrangement
 *   at the displacement/occupation interface; the intra-Israel civic-equality
 *   dimension is a structurally distinct claim left to a separate family
 *   story (see omega epsilon_scope_boundary). The claim/metrics are
 *   independent authored facts: claimed_type snare reflects the structural
 *   read (cover-story coordination, coercion-dependent persistence,
 *   identifiable victims); the metrics describe observed operation. KEY
 *   AGENTS (by structural relationship): - israeli_state_apparatus:
 *   agenda-setting enforcer (institutional/constrained) — administers the
 *   arrangement and collects its gains -
 *   religious_zionist_settlement_movement: primary beneficiary
 *   (organized/identity_locked) — mission-fused constituency receiving land
 *   and legal cover - palestinian_refugee_population: primary target
 *   (powerless/trapped) — bears the founding displacement and its continuing
 *   denial - west_bank_palestinian_residents: primary target
 *   (powerless/trapped) — bears daily costs under military administration -
 *   palestinian_citizens_of_israel: secondary target (moderate/constrained) —
 *   formal inclusion, substantive subordination - levantine_host_states:
 *   excluded parties (institutional/constrained) — bear spillover burdens,
 *   outside the conversation - international_legal_order: analytical observer
 *   (analytical/analytical) — adjudicates without enforcement
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: agenda-setting enforcer (institutional/constrained) — administers the arrangement and collects its gains
 *   - religious_zionist_settlement_movement: primary beneficiary (organized/identity_locked) — mission-fused constituency receiving land and legal cover
 *   - palestinian_refugee_population: primary target (powerless/trapped) — bears the founding displacement and its continuing denial
 *   - west_bank_palestinian_residents: primary target (powerless/trapped) — bears daily costs under military administration
 *   - palestinian_citizens_of_israel: secondary target (moderate/constrained) — formal inclusion, substantive subordination
 *   - levantine_host_states: excluded parties (institutional/constrained) — bear spillover burdens, outside the conversation
 *   - international_legal_order: analytical observer (analytical/analytical) — adjudicates without enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, 0.88).
domain_priors:suppression_score(abrahamic_covenant__land_promise_constraint, 0.9).
domain_priors:theater_ratio(abrahamic_covenant__land_promise_constraint, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, extractiveness, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(abrahamic_covenant__land_promise_constraint, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__land_promise_constraint, snare).
narrative_ontology:human_readable(abrahamic_covenant__land_promise_constraint, "Covenant Territorial Grant as Legitimacy Structure (Land Promise Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__land_promise_constraint, "religious/comparative_theology/institutional_authority").

domain_priors:requires_active_enforcement(abrahamic_covenant__land_promise_constraint).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__land_promise_constraint, 'b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c').
narrative_ontology:cs_kernel_codification('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', fixed_text).
narrative_ontology:cs_authority_grounding('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', extraction).
narrative_ontology:cs_interpretation_layer_present('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c').
narrative_ontology:cs_reading_relation('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', abrahamic_covenant__isaac_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', abrahamic_covenant__ishmael_covenant_reading, forecloses).
narrative_ontology:cs_axiom('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', foundational, everlasting_canaan_grant_to_abrahams_line).
narrative_ontology:cs_axiom_status(everlasting_canaan_grant_to_abrahams_line, holdable).
narrative_ontology:cs_axiom_grounding('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', everlasting_canaan_grant_to_abrahams_line, theological).
narrative_ontology:cs_axiom('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', secondary, divine_grant_confers_territorial_title).
narrative_ontology:cs_axiom_status(divine_grant_confers_territorial_title, holdable).
narrative_ontology:cs_axiom_grounding('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', divine_grant_confers_territorial_title, theological).
narrative_ontology:cs_reference_frame('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', everlasting_canaan_divine_grant).
narrative_ontology:cs_drift_state('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', contemporary_post_1967_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('b3e1a44c-504a-499c-b2f9-3cd70b0a8b2c', '2026-06-12T09:30:00Z').
narrative_ontology:cs_kernel_id(abrahamic_covenant__land_promise_constraint, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__land_promise_constraint, religious_zionist_settlement_movement).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_refugee_population).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(abrahamic_covenant__land_promise_constraint, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, eternal_grant_doctrine).
narrative_ontology:constraint_vindicates(abrahamic_covenant__land_promise_constraint, prophetic_restoration_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the territory between the Jordan and the Mediterranean and the enforcement system that holds it: military command in the occupied territories, land registries, planning committees, and the legal doctrines governing who may build, return, or reside where. Official discourse cites the ancestral promise in declarations, curricula, and ceremonial speech when grounding why the state is where it is and why its borders look as they do. Collects sovereignty, security depth, and the fiscal and electoral value of the settlement enterprise; bears the costs of garrisoning and of diplomatic isolation. Leaving the ancestral-title frame would mean re-founding legitimacy on purely civic terms — available in principle, ruinous in coalition politics.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, agenda_setter,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus, beneficiary).

% A network of yeshivas, youth movements, municipal councils, and settlement organizations that treats residence in the hill-country territories as the active fulfillment of the ancestral deed. Members relocate, build, and hold outposts through demolition-and-retroactive-legalization cycles; their schools teach the deed as literal title. Their self-understanding is constituted by the mission, so leaving the frame is not treated as an option by anyone inside it. They receive land allocations, budgets, and legal defense from the state.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, religious_zionist_settlement_movement, beneficiary,
    organized, generational, identity_locked, regional).

% Families displaced in 1948 and their descendants, registered across camps and diaspora communities in Lebanon, Syria, Jordan, the territories, and further abroad. They hold deeds, keys, and village archives for properties inside Israel; the law of return runs one way, so neither they nor their grandchildren can take up residence. Successive generations inherit the claim as an organizing identity. Host-state laws restrict their employment, property ownership, and movement. Their leverage runs through negotiating bodies and host-state diplomacy rather than any capacity to alter facts on the ground.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_refugee_population, payer,
    powerless, generational, trapped, continental).

% Several million people living under military administration in Areas B and C, where planning permission, water quotas, road access, and residency rights are decided by an authority they cannot vote out. Village lands are declared state land and released to settlements; house demolitions follow permit refusals; movement is channeled through checkpoints. Leaving for work abroad can forfeit residency, so exit means giving up home; staying means absorbing the daily costs. Palestinian Authority documents govern their civil affairs but not the decisions that determine their tenure.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, west_bank_palestinian_residents, payer,
    powerless, biographical, trapped, regional).

% About a fifth of the state's population: voting citizens with parliamentary representation who nonetheless sit outside the ancestral-title conversation. Land and planning regimes, admissions committees, and symbolically constitutional laws assign them a lesser standing in the national home they formally belong to. Individual exit exists — emigration is common enough to have a name — but citizenship, family, and livelihood anchor most in place. They object to the deed-based hierarchy from inside the system that administers it, a position that buys electoral standing and little in land allocation.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(abrahamic_covenant__land_promise_constraint, palestinian_citizens_of_israel, excluded).

% Jordan, Lebanon, and Syria carry the camp systems and the diplomatic weight of the displaced populations on their soil, with domestic coalitions built around either settling or refusing to settle the refugees. They are not parties to the ancestral-title discourse that determines the territory's future; their objections enter only when border security or water allocation forces bilateral negotiation. Geography bounds their options: they cannot relocate the camps or the aquifers.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, levantine_host_states, excluded,
    institutional, generational, constrained, regional).

% Courts, treaty bodies, and UN organs that adjudicate the arrangement's conformity to the Geneva Conventions, the law of occupation, and advisory opinions on the wall, the settlements, and annexation. They issue findings the enforcing parties decline to execute; their seat is analytical, with jurisdictional reach but no enforcement arm. Their docket grows as the arrangement consolidates.
narrative_ontology:constraint_stakeholder(abrahamic_covenant__land_promise_constraint, international_legal_order, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(abrahamic_covenant__land_promise_constraint, israeli_state_apparatus).
narrative_ontology:fixing_cost_class(abrahamic_covenant__land_promise_constraint, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Sustains a dispersed people's cohesion around a shared territorial identity and destination: the deed gives scattered communities a common title-narrative, a common destination, and a reason to endow, migrate, and sacrifice across generations. In the modern arrangement it additionally fuses religious and nationalist constituencies into one governing coalition by supplying a legitimacy formula both accept.
% TRANSFER_FUNCTION: Moves land, housing, water shares, and movement rights from the resident and displaced Palestinian population to the state and the settlement constituency; moves legitimacy — domestic, diaspora, and diplomatic — to whichever governing coalition holds the ancestral-title frame.
% ABSENT_VOICES: The displaced families themselves appear in the conversation only as objects of negotiation, never as interpreters of the deed; the host states carrying the camps are outside the room; dissenting Jewish traditions that read the promise as conditional or spiritual are present in scholarship but not in the enforcing institutions; the international legal order issues findings no seated party must obey. Each is absent from the interpretive authority that decides what the text commands.
% DISAPPEARANCE_RATIONALE: If the ancestral-title frame stopped operating overnight, settlement legalization narratives would lose their anchor, annexation legislation would lose its coalition glue, the refugee claim would shift from theological contest to ordinary legal negotiation, and the governing coalition linking religious and nationalist blocs would dissolve into its parts — the territorial order of the region reorganizes around whatever legitimacy formula replaces it.
% FOUNDING_PROBLEM: Anchor a landless, repeatedly exiled people's claim to one territory and sustain hope of return across two millennia of dispersion — a title-narrative that survives the loss of every political title.
% FOUNDING_PROBLEM_CORROBORATION: Academic biblical scholarship outside the benefiting parties corroborates the original anchoring function (the promise as exilic identity-sustenance); Palestinian historiography and international legal scholarship attest the modern arrangement's function as dispossession-cover; religious-studies literature documents the unresolved conditional/fulfilled/ongoing dispute. Corroboration exists but splits along the same lines as the parties — no neutral seat attests the founding problem is simply live or simply dead.
narrative_ontology:disappearance_verdict(abrahamic_covenant__land_promise_constraint, world_rearranges).
narrative_ontology:founding_problem_status(abrahamic_covenant__land_promise_constraint, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__land_promise_constraint, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(abrahamic_covenant__land_promise_constraint, 'none', 1).
narrative_ontology:epsilon_provenance(abrahamic_covenant__land_promise_constraint, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__land_promise_constraint_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__land_promise_constraint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__land_promise_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.88 at interval end: land, housing, water, and movement rights flow from one population to another under color of divine title, with the rate decoupled from any service the frame renders its payers. Suppression is 0.90 as a raw structural property — military administration, closure of return, permit refusal, checkpoint channeling — and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. Theater ratio 0.42 and rising: performative archaeology, covenant citations in diplomatic fora, and heritage campaigns increasingly overlay a functional coercive core, but the core remains functional, capping the ratio well below piton range. Accessibility_collapse 0.60: rights-based, binational, and compensation frameworks remain fully articulated internationally even while suppressed locally — alternatives are blocked, not unthinkable. Resistance 0.75: two intifadas, sustained litigation, boycott movements, and host-state diplomacy. The suppression series oscillates with uprising cycles (1988, 2000 spikes followed by partial relaxation); the oscillation is not noise — each crisis cycle ends with consolidated facts, so intermittent reinforcement is itself part of the persistence mechanism. Base_properties are measured at interval end (2025, post-consolidation phase). Identity-lock dynamics: the settlement movement's fusion is ideological (the deed constitutes the community's reason for being; exit is unthinkable from inside); the state's fusion is institutional (its self-narrative has become the deed's executor, so reframing legitimacy threatens coalition survival rather than mere policy). Suppression mechanism is overwhelmingly structural (external barriers); a minor internalized component exists among beneficiary constituencies who experience entitled ownership as obvious, which informs but does not drive the scalar.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat compute differently from identical structural data. From the state's and the movement's positions the arrangement is promise-keeping: the same texts, ceremonies, and land transfers that payers experience as dispossession register as fulfillment. From the refugee and West Bank seats the identical structure operates as enforced taking with no exit. The citizens-of-Israel seat computes an intermediate position — included in the polity, subordinated in the land regime. The international legal order sees the whole structure but binds nothing. Same-level divergence: Israel and the host states hold nominally comparable state-level power yet sit at opposite directionalities, differentiated entirely by their relationship to the deed and by exit structure — the host states cannot exit geography, the state cannot exit its coalition. Inter-institutionally, the state and the international legal order collide without resolution because enforcement capacity sits wholly on one side.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the state (agenda-setter and collector) derives a d near the beneficiary end; the settlement movement, identity_locked and subsidized, sits nearer still. Victim declarations drive the targets: refugees and West Bank residents derive d near 1.0, and their trapped exit pins them at the full-target end — no arbitrage-grade exit damps their effective burden. One explicit override: palestinian_citizens_of_israel. Formal citizenship would lead the structural derivation to a mid-range d, but the land and planning regimes that actually allocate costs push their net position to roughly 0.7 — the override corrects the derivation where formal status misleads. Host states carry the excluded role, which is commentary-grade and feeds no correction-grade arithmetic; the international legal order is analytical and likewise outside the d computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — anchoring a stateless, exiled people's claim across two millennia — was genuinely served by the text for centuries, and that genuine service is what makes the modern cover credible; reading the arrangement as pure fiction misses the coordination that gives it traction, while reading it as benign identity coordination misses the asymmetric transfer now flowing through it. The mandate has partially outlived its function: a secure state exists, identity sustains through many channels, and the arrangement's marginal operation now chiefly services expansion-legitimation and coalition maintenance rather than exilic survival. The R5 interview records the founding problem as contested rather than dead — beneficiaries attest liveness through existential-threat framing, outside scholarship attests the shift — so no zombie flag is asserted; the classification instead prevents mislabeling in both directions: not rope (the transfer is real and asymmetric), not piton (enforcement is vigorous and functional, theater well under half), and the snare read rests on the identifiable victim set plus coercion-dependent persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (land_promise_constraint) of the abrahamic_covenant kernel; which structural elements do the sibling readings relocate, and where exactly does the disagreement bind?',
    'Comparative analysis of the three readings'' inheritance clauses and their downstream institutional carriers: which communities, courts, and states treat each reading as operative.',
    'If the isaac_covenant_reading were adopted as sole carrier, the beneficiary set narrows to lineage members without necessarily territorializing; if the ishmael_covenant_reading were adopted, the territorial claim''s exclusivity dissolves and this constraint''s victim set and epsilon restructure entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Committer structure: which reading of the covenant kernel this constraint instantiates and where the readings diverge.').

omega_variable(
    promise_temporal_status,
    'Within this reading, is the grant conditional on conduct, fulfilled in the past, or ongoing and operative?',
    'Textual-tradition analysis (Deuteronomic conditionality strata, rabbinic oath traditions, modern religious-Zionist doctrine) cross-checked against how enforcing institutions actually invoke the text in legislation and court argument.',
    'If fulfilled or merely conditional, the modern territorial-legitimacy invocation loses doctrinal warrant and the arrangement''s epsilon falls toward relic status; if ongoing-unconditional, the legitimacy claim stands and the measured extraction is attributable to the frame itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(promise_temporal_status, conceptual, 'Whether the promise is conditional, fulfilled, or ongoing — the internal axis on which this reading''s modern force turns.').

omega_variable(
    identity_function_separability,
    'Is the territorial component load-bearing for the covenant''s peoplehood-coordination function, or separable cover riding on it?',
    'Counterfactual comparison with non-territorial and conditional-reading Jewish communities that sustain peoplehood without the territorial-exclusivity claim; longitudinal study of diaspora identity maintenance independent of the territorial frame.',
    'If separable, the extraction is cover and the snare reading solidifies; if load-bearing, part of the measured cost is the price of coordination itself and the structure reads as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_function_separability, conceptual, 'Whether the identity-coordination function and the territorial-exclusivity operation are structurally separable.').

omega_variable(
    refugee_coalition_capacity,
    'Can the dispersed refugee constituencies convert demographic weight and host-state leverage into bargaining power sufficient to force renegotiation?',
    'Track right-of-return diplomacy, host-state naturalization politics, and litigation outcomes across a decade; measure whether fragmented representation consolidates.',
    'Effective coalition power raises resistance and could force the arrangement into negotiated coordination, moving the computed classification toward tangled_rope; continued fragmentation pins the targets at full extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_coalition_capacity, empirical, 'Coalition potential of a powerless-class victim set dispersed across multiple host states.').

omega_variable(
    epsilon_scope_boundary,
    'Does this story''s epsilon correctly bound the arrangement at the displacement/occupation interface, or does it leak contribution from the separate intra-Israel civic-equality constraint?',
    'Decomposition review: author the civic-equality dimension as its own family story and verify this story''s metrics are unchanged when the neighbor is scored separately.',
    'If leakage exists, epsilon here is overstated by the neighbor''s contribution; the family decomposition restores epsilon-invariance for both stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_scope_boundary, conceptual, 'Epsilon-invariance guard: confirming the referent boundary of this story''s extractiveness score.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__land_promise_constraint, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t1948, abrahamic_covenant__land_promise_constraint, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(abra_tr_t1948, observed).
narrative_ontology:measurement(abra_tr_t1958, abrahamic_covenant__land_promise_constraint, theater_ratio, 1958, 0.18).
narrative_ontology:measurement_basis(abra_tr_t1958, observed).
narrative_ontology:measurement(abra_tr_t1967, abrahamic_covenant__land_promise_constraint, theater_ratio, 1967, 0.22).
narrative_ontology:measurement_basis(abra_tr_t1967, observed).
narrative_ontology:measurement(abra_tr_t1977, abrahamic_covenant__land_promise_constraint, theater_ratio, 1977, 0.3).
narrative_ontology:measurement_basis(abra_tr_t1977, observed).
narrative_ontology:measurement(abra_tr_t1988, abrahamic_covenant__land_promise_constraint, theater_ratio, 1988, 0.33).
narrative_ontology:measurement_basis(abra_tr_t1988, observed).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__land_promise_constraint, theater_ratio, 2000, 0.35).
narrative_ontology:measurement_basis(abra_tr_t2000, observed).
narrative_ontology:measurement(abra_tr_t2010, abrahamic_covenant__land_promise_constraint, theater_ratio, 2010, 0.38).
narrative_ontology:measurement_basis(abra_tr_t2010, observed).
narrative_ontology:measurement(abra_tr_t2025, abrahamic_covenant__land_promise_constraint, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(abra_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(abra_be_t1948, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement_basis(abra_be_t1948, observed).
narrative_ontology:measurement(abra_be_t1958, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1958, 0.72).
narrative_ontology:measurement_basis(abra_be_t1958, observed).
narrative_ontology:measurement(abra_be_t1967, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement_basis(abra_be_t1967, observed).
narrative_ontology:measurement(abra_be_t1977, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1977, 0.83).
narrative_ontology:measurement_basis(abra_be_t1977, observed).
narrative_ontology:measurement(abra_be_t1988, abrahamic_covenant__land_promise_constraint, base_extractiveness, 1988, 0.85).
narrative_ontology:measurement_basis(abra_be_t1988, observed).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2000, 0.86).
narrative_ontology:measurement_basis(abra_be_t2000, observed).
narrative_ontology:measurement(abra_be_t2010, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2010, 0.87).
narrative_ontology:measurement_basis(abra_be_t2010, observed).
narrative_ontology:measurement(abra_be_t2025, abrahamic_covenant__land_promise_constraint, base_extractiveness, 2025, 0.88).
narrative_ontology:measurement_basis(abra_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t1948, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1948, 0.7).
narrative_ontology:measurement_basis(abra_su_t1948, observed).
narrative_ontology:measurement(abra_su_t1958, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1958, 0.68).
narrative_ontology:measurement_basis(abra_su_t1958, observed).
narrative_ontology:measurement(abra_su_t1967, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1967, 0.74).
narrative_ontology:measurement_basis(abra_su_t1967, observed).
narrative_ontology:measurement(abra_su_t1977, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1977, 0.78).
narrative_ontology:measurement_basis(abra_su_t1977, observed).
narrative_ontology:measurement(abra_su_t1988, abrahamic_covenant__land_promise_constraint, suppression_requirement, 1988, 0.84).
narrative_ontology:measurement_basis(abra_su_t1988, observed).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement_basis(abra_su_t2000, observed).
narrative_ontology:measurement(abra_su_t2010, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2010, 0.86).
narrative_ontology:measurement_basis(abra_su_t2010, observed).
narrative_ontology:measurement(abra_su_t2025, abrahamic_covenant__land_promise_constraint, suppression_requirement, 2025, 0.9).
narrative_ontology:measurement_basis(abra_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__land_promise_constraint, identity_coordination).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, abrahamic_covenant__isaac_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__land_promise_constraint, abrahamic_covenant__ishmael_covenant_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Abrahamic covenant' bundles at least three structurally distinct claims: lineage transmission through Isaac, continuation or inclusion through Ishmael, and a territorial grant of Canaan. Each carries its own epsilon, beneficiary/victim structure, and enforcement interface; forcing one story to average across them would make epsilon observer-relative, violating decomposition discipline. This file authors the territorial-grant reading. Lineage determinations condition who could inherit a grant, so the territorial reading is downstream of the lineage readings' exclusivity structure; the upstream claims are typically cited as warrant for the downstream territorial claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(abrahamic_covenant__land_promise_constraint, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
