% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__continuity_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: orthographic_legitimacy_kernel__continuity_reading
 *   human_readable: Orthographic Continuity: Preserving Access to Pre-Reform Literary Canon
 *   domain: political_linguistics/state_formation
 *
 * SUMMARY:
 *   This constraint is one reading of a contested orthographic kernel: what
 *   makes an orthographic system legitimate? The continuity reading asserts
 *   that legitimacy derives from preserving access to historical, religious,
 *   and literary tradition. When a state reforms its script—shifting from
 *   Arabic to Latin alphabets, for instance—the reform creates a structural
 *   incompatibility: the new generation learns the new script and gains
 *   modern literacy, but simultaneously loses unmediated access to centuries
 *   of texts written in the old script. This reading emphasizes that loss as
 *   the constraint's core: the post-reform generation inherits a library it
 *   cannot directly read without additional training. The constraint is
 *   mountain-like because the incompatibility is a physical fact of script
 *   systems, not a human choice per se. However, it is called a constraint
 *   story (not simply a natural law) because the decision to preserve or
 *   abandon continuity is political, and the legitimacy claim that continuity
 *   *matters* is contested. The other readings (modernist and
 *   instrumentalist) would author different ε values by refusing the
 *   continuity claim as a metric of legitimacy.
 *
 * KEY AGENTS:
 *   - post_reform_generations: the victims of the access barrier; born into the new script and identity-locked to it; cognitive burden of dual-script literacy falls on them
 *   - arabic_script_custodians: the keepers of old-script fluency; they maintain the living possibility of access; their knowledge is the measured testimony to what continuity requires
 *   - religious_continuity_advocates: organized to preserve tradition; they benefit from the legitimacy claim that continuity matters and resources flow to manuscript preservation
 *   - political_reformers: the agenda-setters who chose the orthographic shift; they do not extract from continuity loss but they set the constraint that creates it
 *   - colonial_observers: external analytical seat; they document the structure without claiming legitimacy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__continuity_reading, 0.35).
domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, 0.62).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__continuity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__continuity_reading, mountain).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__continuity_reading, "Orthographic Continuity: Preserving Access to Pre-Reform Literary Canon").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__continuity_reading, "political_linguistics/state_formation").

domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__continuity_reading, '825d3143-5876-4996-91eb-da06f82615d7').
narrative_ontology:cs_kernel_codification('825d3143-5876-4996-91eb-da06f82615d7', distributed).
narrative_ontology:cs_authority_grounding('825d3143-5876-4996-91eb-da06f82615d7', distributed).
narrative_ontology:cs_reading_relation('825d3143-5876-4996-91eb-da06f82615d7', orthographic_legitimacy_kernel__instrumentalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('825d3143-5876-4996-91eb-da06f82615d7', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('825d3143-5876-4996-91eb-da06f82615d7', foundational, continuity_with_pre_reform_canon_is_legitimacy_ground).
narrative_ontology:cs_axiom_status(continuity_with_pre_reform_canon_is_legitimacy_ground, holdable).
narrative_ontology:cs_axiom_grounding('825d3143-5876-4996-91eb-da06f82615d7', continuity_with_pre_reform_canon_is_legitimacy_ground, deontological).
narrative_ontology:cs_axiom('825d3143-5876-4996-91eb-da06f82615d7', secondary, access_to_historical_texts_constitutes_cultural_identity).
narrative_ontology:cs_axiom_status(access_to_historical_texts_constitutes_cultural_identity, holdable).
narrative_ontology:cs_axiom_grounding('825d3143-5876-4996-91eb-da06f82615d7', access_to_historical_texts_constitutes_cultural_identity, deontological).
narrative_ontology:cs_reference_frame('825d3143-5876-4996-91eb-da06f82615d7', biliteracy_maintained_continuity).
narrative_ontology:cs_drift_state('825d3143-5876-4996-91eb-da06f82615d7', contemporary_monolingual_cohort, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('825d3143-5876-4996-91eb-da06f82615d7', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__continuity_reading, religious_continuity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious scholars, manuscript librarians, and historians who maintain fluency in Ottoman and Islamic-era texts. They do not extract from the orthographic regime; they preserve access to it. Their voice is the authoritative testimony to what continuity with pre-1928 texts requires.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, arabic_script_custodians, observer,
    moderate, generational, constrained, national).

% Born after orthographic reform, schooled entirely in the new script. They inherit a rupture: fluency in contemporary literacy does not confer access to the library of pre-1928 texts, religious manuscripts, legal records, or poetry. Recovering that access requires learning a second script system as adults, at substantial cognitive and time cost. Their identity as nationals is constituted through the reformed script; exiting that identity is not a realistic option.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, post_reform_generations, payer,
    powerless, biographical, identity_locked, national).

% State actors who selected and enforced the script reform as modernization policy. They justify the reform by literacy gain, administrative efficiency, and alignment with European modernity. They do not directly extract from orthographic continuity loss, but they set and defend the constraint that creates it.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, political_reformers, agenda_setter,
    institutional, generational, arbitrage, national).

% Communities and institutions committed to preserving access to pre-reform religious texts and interpretive traditions. They benefit from the constraint because it preserves the canonical status of texts written in the old script and honors the legitimacy claim that orthography must serve tradition continuity. They can organize resources to maintain manuscript preservation and dual-script literacy without depending on state enforcement.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, religious_continuity_advocates, beneficiary,
    organized, civilizational, mobile, national).

% External analysts examining whether the reform represents modernization or cultural rupture. They do not stake a claim inside the constraint; they observe how different stakeholders experience the orthographic reorganization and document the structural consequences.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__continuity_reading, colonial_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(orthographic_legitimacy_kernel__continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves organized access to the literary, religious, and historical canon accumulated before 1928: thousands of years of Islamic jurisprudence, Ottoman administrative texts, classical poetry, theological interpretation. The constraint is that if the script changes, that library becomes inaccessible to new learners unless they undergo additional training in the old script.
% TRANSFER_FUNCTION: The constraint transfers cognitive burden: post-reform generations inherit full literacy in the new script (enabling contemporary communication and administration) but must pay a learning cost to access pre-reform tradition. The transfer is not economic; it is a reallocation of who bears the cost of script knowledge. The old-script custodian class carries knowledge maintenance; the new-script generation carries the access barrier.
% ABSENT_VOICES: The pre-1928 population is absent by definition—they are dead. Their descendants might claim that the reform breaks lineage and cuts them off from their own ancestors' words, but that voice is only audible through the surviving manuscript tradition, not as a party to the reform decision.
% DISAPPEARANCE_RATIONALE: If the constraint—the standard that orthographic legitimacy requires preserving access to pre-reform tradition—disappeared, the state could abandon dual-script literacy and allow the old script to fade from institutional knowledge. The manuscript libraries would remain, but their social significance would shift from 'living tradition' to 'historical artifact.' Different stakeholders contest whether the world would rearrange: reformers say no, literacy gains and modernity persist; continuity advocates say yes, because access to tradition constitutes religious and national identity.
% FOUNDING_PROBLEM: The problem was state modernization and administrative consolidation: Ottoman script diversity (Arabic script variants, regional adaptations, administrative shorthand) created barriers to literacy standardization and national print communication. A single, phonetically efficient script was seen as necessary to build a modern state and integrate the population under one written standard.
% FOUNDING_PROBLEM_CORROBORATION: The reformers and efficiency-focused administrators attest the founding problem is live: literacy rates were low, administration was fragmented, and Ottoman regional script variants persisted. Historians and continuity advocates attest the founding problem is a *framing*: the barriers to standardized literacy existed not because script diversity was inherently inefficient but because the empire had not invested in universal education, and the reform accelerated access for the new generation at the cost of severing the old. They cite UNESCO documentation and comparative literacy studies showing similar literacy gains through other means without orthographic rupture.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__continuity_reading, contested).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__continuity_reading, 0.35, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, ExtMetricName, E),
    domain_priors:suppression_score(orthographic_legitimacy_kernel__continuity_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(orthographic_legitimacy_kernel__continuity_reading),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__continuity_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(orthographic_legitimacy_kernel__continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.35 by interval end) because this reading does not identify a concentrated beneficiary extracting rent from the constraint. Instead, it identifies a diffuse cost imposed on the post-reform generation: the loss of direct access. The suppression value (0.62) reflects that enforcement is required: the state must actively defend the new script's primacy in education and administration, suppressing the transmission of old-script fluency to new generations. Suppressiveness does not vanish because the constraint persists only if new generations are prevented from treating the old script as 'normal' or co-equal. Theater ratio (0.48, stabilizing near 0.48) reflects that the state's justification for the reform (literacy gain, modernity, efficiency) is partly true but partly a cover story for the continuity loss imposed. The measurement series shows extractiveness rising in the first 50 time points as the full generational rupture becomes visible, then plateauing: by time 100, the new generations are the overwhelming demographic majority and the constraint is 'naturalized'—it no longer feels like an active policy choice but like the way writing 'simply is.' Resistance is high (0.71) because continuity advocates mount sustained institutional and cultural pushback, maintaining dual-script literacy and manuscript preservation as acts of defiance.
 *
 * PERSPECTIVAL GAP:
 *   From the reformers' seat, the constraint is a foundation of modernity and national unity—a public good. From the post-reform generation's seat, it is a barrier to their own history. From the custodian's seat, it is a continuous struggle to keep tradition alive against institutional entropy. The engine should compute different type assignments per seat: the reformer may experience this as rope (genuine coordination for literacy), while the post-reform generation experiences it as snare (severed access, suppressed alternatives). The authored claimed_type (mountain) reflects the physical incompatibility fact itself; the computed types per seat will diverge from the claim.
 *
 * DIRECTIONALITY LOGIC:
 *   The post-reform generations are the targets (high d, near 1.0): they bear the access cost and are identity-locked to the new script—they cannot exit by learning the old script as a child because it is not taught to them as a native system. The religious continuity advocates are beneficiaries in a structural sense (low d, near beneficiary end): they benefit from the legitimacy claim that tradition matters and from resources devoted to preservation. But they are not extracting in the economic sense—they are defending something they hold to be valuable. The political reformers are agenda-setters but not extractors in the Snare sense: they imposed the constraint but did not capture it for private gain. The custodians (arabic_script_custodians) are observers with constrained exit: they keep the old script alive but do not control the state's policy. This reading's directionality structure is asymmetric but not economically extractive—it is a loss of access suffered by the powerless (post-reform generations) and a burden of maintenance borne by the organized (custodians and advocates).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state modernization and literacy standardization) is contested. Reformers say it is live—administrative fragmentation still exists without a unified script. Continuity advocates and historians say the problem is a framing, and the actual founding problem was never script efficiency but rather the state's choice to abandon investment in biliteracy. The constraint does not appear to resolve a mandated function by 1928 standards, yet it persists. This suggests either that the constraint is capturing value (reformers continue to benefit from unified script dominance and symbolic control of national literacy) or that it is inertial (the constraint persists because reversing it is politically impossible, not because anyone actively maintains it). The measurement series shows theater_ratio stabilizing around 0.48—neither predominantly functional nor predominantly theatrical—which suggests the constraint is neither fully justified by its original mandate nor purely performative. The mandatrophy_resolved flag should be false: the constraint's mandate (script standardization) persists as an active state function, even if contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mountain_vs_constructed_ambiguity,
    'Is the access barrier to pre-reform texts a natural, inevitable consequence of script incompatibility (mountain), or a constructed constraint chosen by the reform planners who could have designed dual-script education instead?',
    'Historical counterfactual analysis: compare societies that reformed script while maintaining dual-literacy education (Singapore, Norway post-Nynorsk adoption, Japan with kanji/hiragana) against those that abandoned old-script teaching entirely. If dual-literacy is feasible and is chosen against, the barrier is constructed, not natural.',
    'If natural/mountain: the constraint is a cost of modernity that post-reform generations must bear; ε remains low and no agent is culpable. If constructed/tangled_rope: the reformers chose not to maintain dual-literacy; extraction is the diffuse cost imposed on those without choice; ε may rise to 0.45+. The classification hinges on this ambiguity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mountain_vs_constructed_ambiguity, empirical, 'Is script incompatibility inevitable or engineered?').

omega_variable(
    victim_identity_lock_mechanism,
    'Is the post-reform generation identity-locked to the new script because they are developmentally trapped (cannot relearn the old script as efficiently as a child), or because the state actively suppresses old-script literacy and constitutes national identity around the new script, making the lock internalized rather than structural?',
    'Policy experiment: would lifting the state''s suppression of old-script education substantially increase its adoption among post-reform generations? If yes, the lock is partly internalized (state narrative + enforcement together). If adoption remains low even after lifting suppression (due to economic opportunity cost and network effects), the lock is structural (physical realities of adult language learning).',
    'If internalized: suppression is the causal agent; the constraint is snare-like and requires enforcement to persist. If structural: the constraint is more mountain-like; reformers imposed it once, and it persists without active enforcement. This splits suppression mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_identity_lock_mechanism, empirical, 'What is the mechanism by which post-reform generations are locked into the new script?').

omega_variable(
    legitimacy_grounding_contest,
    'Does orthographic legitimacy derive from preserving access to tradition (continuity reading), from maximizing literacy rates and administrative efficiency (instrumentalist reading), or from alignment with Western/European modernity (modernist reading)? The three readings cannot coexist in a single framework—they make incompatible claims about what counts as legitimate.',
    'This is a conceptual ambiguity. The resolution is not empirical discovery but committer choice: different political communities will endorse different readings. However, the empirical question ''which reading is actually guiding state policy and cultural institutions?'' can be assessed by examining what the state allocates resources to (if preservation of old-script access, the continuity reading is being followed; if only new-script standardization, the instrumentalist reading is active).',
    'If the continuity reading is correct, the constraint preserves something of value and ε is correctly set low; the loss is tragic but not extractive. If the modernist reading is correct, the constraint is an intentional severance from Islamic/Ottoman tradition, and ε should rise (erasure becomes a feature, not a cost). If the instrumentalist reading is correct, ε should be near zero (efficiency gain outweighs any access loss).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_grounding_contest, conceptual, 'Which grounding of orthographic legitimacy is structurally true?').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.62) structural—enforced by state policy, curriculum design, and lack of old-script literacy instruction—or is it internalized in post-reform generations'' belief that the old script is archaic, irrelevant, or foreign?',
    'Post-exit trajectory: if access to old-script education were made freely available (e.g., religious schools offering dual-script teaching), would post-reform generations adopt it? If adoption rates remain low even when barriers are removed, suppression is partly internalized (the target carries the suppression with them).',
    'If structural: the constraint''s effective suppression is accurate as measured; fixing it requires changing state policy. If internalized: the constraint''s effective suppression is higher than the structural measure; even after policy change, the internalized belief system persists, and the constraint''s hold is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Is suppression structural or internalized in the target generation?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__continuity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(orth_tr_t0, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement_basis(orth_tr_t0, observed).
narrative_ontology:measurement(orth_tr_t10, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement_basis(orth_tr_t10, observed).
narrative_ontology:measurement(orth_tr_t25, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(orth_tr_t25, observed).
narrative_ontology:measurement(orth_tr_t50, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 50, 0.48).
narrative_ontology:measurement_basis(orth_tr_t50, observed).
narrative_ontology:measurement(orth_tr_t75, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 75, 0.48).
narrative_ontology:measurement_basis(orth_tr_t75, observed).
narrative_ontology:measurement(orth_tr_t100, orthographic_legitimacy_kernel__continuity_reading, theater_ratio, 100, 0.48).
narrative_ontology:measurement_basis(orth_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(orth_be_t0, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(orth_be_t0, observed).
narrative_ontology:measurement(orth_be_t10, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement_basis(orth_be_t10, observed).
narrative_ontology:measurement(orth_be_t25, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 25, 0.3).
narrative_ontology:measurement_basis(orth_be_t25, observed).
narrative_ontology:measurement(orth_be_t50, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(orth_be_t50, observed).
narrative_ontology:measurement(orth_be_t75, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 75, 0.37).
narrative_ontology:measurement_basis(orth_be_t75, observed).
narrative_ontology:measurement(orth_be_t100, orthographic_legitimacy_kernel__continuity_reading, base_extractiveness, 100, 0.35).
narrative_ontology:measurement_basis(orth_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(orth_su_t0, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(orth_su_t0, observed).
narrative_ontology:measurement(orth_su_t10, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 10, 0.58).
narrative_ontology:measurement_basis(orth_su_t10, observed).
narrative_ontology:measurement(orth_su_t25, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 25, 0.62).
narrative_ontology:measurement_basis(orth_su_t25, observed).
narrative_ontology:measurement(orth_su_t50, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 50, 0.62).
narrative_ontology:measurement_basis(orth_su_t50, observed).
narrative_ontology:measurement(orth_su_t75, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 75, 0.62).
narrative_ontology:measurement_basis(orth_su_t75, observed).
narrative_ontology:measurement(orth_su_t100, orthographic_legitimacy_kernel__continuity_reading, suppression_requirement, 100, 0.62).
narrative_ontology:measurement_basis(orth_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__continuity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(orthographic_legitimacy_kernel__continuity_reading, 0.12).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__instrumentalist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__continuity_reading, orthographic_legitimacy_kernel__modernist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the orthographic_legitimacy_kernel. The kernel is the contested legitimacy claim: what makes an orthographic system legitimate? The continuity reading asserts legitimacy derives from preserving access to pre-reform tradition; the instrumentalist reading asserts it derives from literacy efficiency; the modernist reading asserts it derives from rupture with Ottoman/Islamic past. The three constraints have different ε values, beneficiary/victim structures, and classifications precisely because they answer the legitimacy question differently. They are linked by network.affects_constraints as a constraint family. The three readings cannot coexist in a single framework—they are genuinely incompatible answers to the same kernel question. Each constraint story models one reading as if it were the true grounding of legitimacy; the engine will classify each from the reading's own lights. The sibling readings are OTHER constraints, not alternate observations of this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(orthographic_legitimacy_kernel__continuity_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
