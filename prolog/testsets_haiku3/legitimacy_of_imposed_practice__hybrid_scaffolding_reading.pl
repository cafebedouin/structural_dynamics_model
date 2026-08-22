% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__hybrid_scaffolding_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__hybrid_scaffolding_reading
 *   human_readable: Scaffolded Practice Displacement via Elite Modeling and Ideological Messaging
 *   domain: political/cultural/social
 *
 * SUMMARY:
 *   A state issues a top-down mandate to displace an existing social practice
 *   (e.g., calendar reform, dress code, language standardization, religious
 *   observance) deemed incompatible with the state's vision of modernity,
 *   national identity, or administrative efficiency. The mandate alone fails
 *   — pure decree without internalization produces only surface compliance.
 *   However, when paired with scaffolding (educational infrastructure, media
 *   representation, official modeling by elites) and ideological messaging
 *   that frames the new practice as progress or national pride, the mandate
 *   achieves partial displacement. Elites adopt the new practice readily and
 *   perform it as a marker of cosmopolitan identity. Rural and subaltern
 *   communities lack scaffolding access and face enforcement pressure,
 *   resulting in hybrid practices, hidden persistence of the prior practice,
 *   or resentful formal compliance. This constraint is a hybrid of tangled
 *   rope (genuine coordination function + asymmetric extraction) and snare
 *   (persistence via enforcement and suppression of alternatives, not via
 *   internalized preference). The key mechanism is the conversion of decree
 *   into quasi-endogenous pull through ideological apparatus, creating the
 *   appearance that adoption is natural or inevitable rather than imposed.
 *
 * KEY AGENTS:
 *   - state_authority: agenda-setter, controls mandate and scaffolding infrastructure
 *   - urban_elites: beneficiary, adopt first and perform new practice as status marker
 *   - rural_populations: payer, lack scaffolding access, face enforcement, slow/reluctant adoption
 *   - subaltern_communities: payer and identity-victimized, prior practice is identity-constitutive
 *   - ideological_apparatus: non-agent, mechanism of quasi-endogenous conversion
 *   - enforcement_machinery: non-agent, mechanism of compliance-coercion
 *   - prior_practice_custodians: excluded, would object but lack institutional voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.68).
domain_priors:suppression_score(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.72).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, tangled_rope).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "Scaffolded Practice Displacement via Elite Modeling and Ideological Messaging").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, "political/cultural/social").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__hybrid_scaffolding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f').
narrative_ontology:cs_kernel_codification('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', distributed).
narrative_ontology:cs_authority_grounding('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', extraction).
narrative_ontology:cs_interpretation_layer_present('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f').
narrative_ontology:cs_reading_relation('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', legitimacy_of_imposed_practice__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', legitimacy_of_imposed_practice__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', foundational, scaffolding_enables_displacement).
narrative_ontology:cs_axiom_status(scaffolding_enables_displacement, holdable).
narrative_ontology:cs_axiom_grounding('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', scaffolding_enables_displacement, empirically_contingent).
narrative_ontology:cs_axiom('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', foundational, ideological_messaging_converts_decree_to_quasi_endogeneity).
narrative_ontology:cs_axiom_status(ideological_messaging_converts_decree_to_quasi_endogeneity, holdable).
narrative_ontology:cs_axiom_grounding('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', ideological_messaging_converts_decree_to_quasi_endogeneity, empirically_contingent).
narrative_ontology:cs_reference_frame('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', decree_backed_by_state_authority).
narrative_ontology:cs_drift_state('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', post_initial_displacement, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('ffaa67a6-31ba-4a59-a605-bcdf8cb9ce9f', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_authority).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, subaltern_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the top-down mandate to displace the prior practice (e.g., calendar reform, dress code, language standardization). Constructs the scaffolding infrastructure (education, media, official models) and deploys ideological messaging linking the new practice to progress, modernity, or national identity. Maintains enforcement through administrative channels and legitimacy claims. Profits from the displacement through cultural standardization and state capacity consolidation.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% First to adopt the new practice because they have access to the scaffolding infrastructure (schools, publications, official circles) and can afford the transition costs. The adoption becomes a marker of cosmopolitan identity and social status. They experience the mandate as aspirational rather than coercive — they benefit from the cultural shift and the distinction it confers. They may nominally adopt but retain hybrid practices (code-switching between old and new contexts).
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, urban_elites, payer).

% Located geographically and institutionally outside the scaffolding infrastructure. Schools and media reaching them are sparse or late-arriving; cost of transition (learning new practices, acquiring new materials) is higher relative to income. Face enforcement pressure (legal penalty, social censure, administrative discrimination) if they maintain prior practices. Lack access to the hybrid identity markers that allow elites to navigate both worlds. Internalize the new practice slowly, often under duress, sometimes never fully.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, rural_populations, payer,
    powerless, biographical, identity_locked, local).

% Communities whose prior practice is targeted for displacement and whose identity is constituted through that practice (language speakers, religious practitioners, cultural custodians). The mandate threatens identity continuity, not merely daily habit. They face the same enforcement pressure as rural populations plus the additional burden of defending a practice that the state and urban elites delegitimize as backward. Hybrid adoption is often experienced as cultural loss rather than enrichment.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, subaltern_communities, payer,
    powerless, biographical, identity_locked, local).

% Communities organized around the transmission and maintenance of prior practices (language teachers, religious leaders, cultural practitioners). They are excluded from the mandate-setting process and from the ideological apparatus that frames the displacement. They have direct interest in preserving the prior practice but lack institutional power to resist. Their exclusion is structural to the scaffolded imposition — the apparatus cannot function if they are seated.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prior_practice_custodians, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, state_authority).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Displaces a prior practice deemed incompatible with the state's vision of national identity, modernization, or administrative standardization. Without scaffolding, the coordination problem is unsolved and the decree fails; with scaffolding, the state coordinates a shift in which elite identity becomes invested in the new practice, and enforcement handles outliers.
% TRANSFER_FUNCTION: Moves cultural authority and social status markers from subaltern and rural communities (who hold the prior practice) to urban elites (who adopt the new practice first and perform cosmopolitan identity). Moves enforcement costs downward (rural and powerless populations bear the cost of adapting; elites bear the cost of the scaffolding infrastructure, which they control). Moves legitimacy claims from the prior practice to the new one — a reframing of what counts as civilization.
% ABSENT_VOICES: Prior practice custodians, language speakers, religious practitioners, and rural communities are structurally excluded from the mandate-setting process. They would testify that the displacement is coercive, that scaffolding access is unequal, and that the ideological narrative misrepresents their practices as backward rather than legitimate. Their exclusion is necessary to the scaffolded imposition — including them would surface the mandate as decree rather than allow it to masquerade as natural evolution.
% DISAPPEARANCE_RATIONALE: If the mandate, scaffolding, and enforcement machinery vanished, the prior practice would resurge in rural and subaltern communities almost immediately; urban elites would face a choice between maintaining the new practice (now costly and delegitimized) or reverting to the prior one (now carrying lower status). The regime's symbolic unity around the new practice would fragment. The disappearance would reveal that displacement rested on active suppression and messaging, not internalized preference.
% FOUNDING_PROBLEM: The prior practice is incompatible with the state's administrative or ideological vision (state standardization, national unification, modernization narrative, religious or cultural conformity). The founding problem assumes the prior practice is an obstacle, not a legitimate alternative.
% FOUNDING_PROBLEM_CORROBORATION: State authorities and urban elites attest the prior practice is an obstacle to modernization or national unity. Anthropologists, linguists, historians, and rural communities outside the benefiting parties attest that the prior practice is functional and legitimate within its context, and that the 'incompatibility' is a constructed judgment serving state and elite interests, not a structural fact. Legislative debates and post-displacement empirical records show that the prior practice continues in hidden or hybrid forms, suggesting it was not the obstacle the narrative claimed.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimacy_of_imposed_practice__hybrid_scaffolding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises sharply in the first 30 years (0.35→0.68) as the scaffolding infrastructure matures and adoption accelerates, then plateaus and slightly declines (0.68→0.68 at year 50) as the mandate becomes normalized and enforcement pressure eases — a characteristic tangled-rope lifecycle where active extraction peaks when resistance is highest and declines when normalization removes the need for constant pushing. Theater ratio traces a similar arc but remains high (0.58 at endpoint), indicating persistent performative maintenance: the ideological apparatus continues to frame the new practice as natural, even after adoption is widespread and enforcement pressure has eased. This is the signature of a scaffolded imposition — ongoing theatrical work to prevent reversion and to maintain the constructed naturalness of what was imposed. Suppression requirement follows extractiveness with a lag, rising from 0.45 to 0.74 as resistance hardened mid-transition, then declining slightly as enforcement machinery becomes institutionalized and less visible. The measurement series are authored on one shared time grid (every metric at every time point) to prevent the OQ-105 misalignment artifact. The slight downturn in extractiveness and theater ratio at year 50 reflects the endpoint state: post-displacement, the constraint has become a (partially successful) snare — the prior practice is suppressed, elites have internalized the new practice as identity, and rural populations have adapted (some genuinely, some performatively). The ongoing theater ratio (0.58) indicates the constraint remains vulnerable to reversion without continued ideological maintenance.
 *
 * PERSPECTIVAL GAP:
 *   From the state authority and urban elites' position, the constraint is genuine coordination solving a real incompatibility (modernization requires standardization, national unity requires common symbols). From rural and subaltern communities' position, the same structure is enforced cultural erasure hidden behind progress rhetoric. The engine computes this divergence from the structural data: state authority and elites see low d (beneficiary end) and compute low/negative χ; rural populations see high d (target end) and compute high χ. The gap is not a measurement error — it is the constraint's true structural asymmetry. The key insight is that ideological messaging (the scaffolding apparatus) allows the state to narrate the constraint as coordination when, from the victim perspective, it is coercive displacement.
 *
 * DIRECTIONALITY LOGIC:
 *   State authority is a full beneficiary: it consolidates administrative capacity, achieves cultural standardization, and profits from the ideological narrative's legitimation of its authority. Urban elites are partial beneficiaries (they gain status and identity-capital from adopting first) and partial payers (they bear the cost of the scaffolding infrastructure and must perform the new practice, though the performance is voluntary for them because they have identity capital to deploy). Rural populations and subaltern communities are pure payers: they lack scaffolding access (constrained exit), face enforcement pressure (trapped or identity-locked status), and experience the displacement as loss of identity or autonomy. The directionality derivation flows directly from beneficiary/victim declarations plus exit options: state authority (beneficiary, arbitrage options) → d near 0.0; urban elites (beneficiary, mobile exit) → d near 0.2; rural populations (victim, identity-locked exit) → d near 0.85; subaltern communities (victim, trapped identity) → d near 0.9. No overrides are needed; the structural data produces the correct directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy — the founding problem (prior practice incompatibility) is contested rather than live. The state and elites attest it is live; outside observers and affected communities attest it is either solved (the incompatibility never existed, it was constructed) or dead (the prior practice could coexist with modernity). The theater ratio at 0.58 and the ongoing enforcement requirement both support the mandatrophy reading: the constraint persists through ongoing ideological work (theater) and enforcement machinery, not through alignment with a live coordinating problem. If the founding problem were live, we would expect clearer evidence of actual incompatibility rather than performative assertions. The measurement series showing theater ratio rising as extractiveness rises, then both declining and plateauing, traces a classic mandatrophy arc: early high extraction + high theater (decree + messaging fighting resistance), later lower extraction + still-high theater (normalized suppression + ongoing ideological maintenance). The slight uptick in theater ratio from year 40 to 50 (despite stable extractiveness) suggests the constraint's memory of its fragility — the need for continued performance to prevent reversion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scaffolding_sufficiency_vs_coercion,
    'How much of the observed adoption is driven by access to scaffolding (quasi-endogenous pull from elite modeling and institutional infrastructure) versus by enforcement pressure and threat of penalty (coercive pressure)? Can we decompose the mechanism?',
    'Natural experiment: compare adoption rates in regions with high scaffolding access vs. low scaffolding access controlling for enforcement intensity. If adoption diverges significantly, scaffolding is a distinct mechanism; if enforcement is the primary driver regardless of scaffolding access, the constraint is closer to pure coercion than hybrid scaffolding.',
    'If scaffolding is a genuine distinct mechanism (not just theater), the constraint deserves the tangled_rope classification (coordination + extraction); if scaffolding is primarily ideological cover for coercion, the constraint reclassifies toward snare (pure extraction). The endpoint ε (0.68) assumes hybrid mechanism; pure coercion would be lower-theater, higher-naked-enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffolding_sufficiency_vs_coercion, empirical, 'Decomposition of scaffolding-driven vs. enforcement-driven adoption.').

omega_variable(
    ideological_apparatus_internalization,
    'How much of the ideological messaging''s success is genuine persuasion (the new practice is internalized as good or inevitable) versus performative adoption (the new practice is adopted because elites perform it and non-adoption is stigmatized)? Do adopters internalize the new practice as identity, or merely enact it in public?',
    'Post-displacement ethnographic observation and linguistic anthropology: do communities maintain the prior practice in private spaces, switch code between public and private contexts, or fully internalize the new practice? Do subaltern communities raise children in the new practice or teach both? Do rural communities resist or gradually adopt the ideological narrative about the new practice?',
    'If internalization is high, the constraint has achieved durable displacement and tangled_rope classification is stable. If internalization is low and the prior practice persists hidden, the constraint is closer to a snare maintained by enforcement and appearance, vulnerable to reversion. The theater ratio (0.58) suggests partial internalization; if it is actually low, theater should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_apparatus_internalization, empirical, 'Distinction between genuine internalization and performative public adoption.').

omega_variable(
    sibling_reading_observability,
    'If we could observe a pure-decree scenario (mandate without scaffolding or ideology) and a pure-climb scenario (no state mandate, but spontaneous elite adoption that cascades), would they produce observably different adoption curves and persistence profiles compared to the scaffolded imposition we see here?',
    'Historical/cross-national comparison: find cases where one sibling mechanism operated in isolation and trace adoption curves. Compare calendar reform imposed without ideological framing to language shift driven by elite preference without state mandate.',
    'If the adoption curves are observably distinct, the three sibling readings are empirically discriminable and the hybrid_scaffolding_reading is a valid structural claim. If adoption curves converge, the three readings may be observational variants of the same underlying mechanism, and the kernel contest is more about framing than structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_observability, empirical, 'Observational distinctness of the three sibling reading mechanisms.').

omega_variable(
    hybrid_practice_escape_valve,
    'Do hybrid practices (code-switching, syncretism, maintaining prior practice in hidden contexts) represent a stable adaptation or a gateway to eventual reversion? Is the hybrid practice a ''resting point'' in the displacement process, or a precarious performance that will collapse if enforcement pressure eases?',
    'Generational tracking: observe whether children of code-switchers adopt the new practice fully, maintain hybridity, or revert to the prior practice. Track whether hybrid practice visibility increases when enforcement pressure declines.',
    'If hybrid practices are stable resting points, the constraint has achieved a durable if contested displacement (tangled_rope with high theater). If hybrid practices are precarious and collapse without enforcement, the constraint is closer to a snare and highly vulnerable to institutional change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_practice_escape_valve, empirical, 'Stability and trajectory of hybrid practice adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 20, 0.52).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 30, 0.58).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 40, 0.61).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 40, 0.71).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__hybrid_scaffolding_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, 0.12).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__endogenous_climb_reading).
narrative_ontology:affects_constraint(legitimacy_of_imposed_practice__hybrid_scaffolding_reading, legitimacy_of_imposed_practice__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the contested kernel 'legitimacy_of_imposed_practice'. The kernel asks: what mechanism causes practice displacement? The hybrid_scaffolding_reading answers: top-down mandate + scaffolding + ideological messaging. The endogenous_climb_reading answers: internalization via bottom-up adoption pathways. The exogenous_override_reading answers: state decree authority alone. Each sibling reading has a different ε value for its respective mechanism and should be authored as a separate constraint story. They are linked via network.affects_constraints to signal that they are kin claims about the same underlying kernel, not independent constraints. The kernel contest is active across multiple disciplines (history, anthropology, political science, linguistics); these constraint stories capture the structural claims each reading makes about how practice displacement occurs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
