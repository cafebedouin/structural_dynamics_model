% ============================================================================
% CONSTRAINT STORY: divine_legitimacy_substrate__folk_syncretistic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_divine_legitimacy_substrate__folk_syncretistic_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: divine_legitimacy_substrate__folk_syncretistic_reading
 *   human_readable: Folk Syncretistic Reading of Divine Legitimacy (Household/Village Ritual Practice)
 *   domain: Ancient History / Religious Studies / Political Economy of Belief Systems
 *
 * SUMMARY:
 *   This story instantiates the folk-syncretistic reading of the divine
 *   legitimacy substrate kernel: legitimacy is understood to arise from the
 *   diffuse, pragmatic, household- and village-level ritual practice that
 *   incorporates multiple deities according to local need and perceived
 *   efficacy, rather than from priestly cosmological interpretation (the
 *   amun_polytheistic_reading) or pharaonic exclusive revelation (the
 *   atenist_monotheistic_reading). Under this reading, authority is not
 *   centrally administered at all — it is enacted continuously and
 *   independently at thousands of household altars, and neither temple nor
 *   crown can fully observe, correct, or extract from it. This is a distinct
 *   constraint from its siblings, not a different observable angle on the
 *   same one: its ε is low because there is no concentrated apparatus
 *   collecting rents from it, its beneficiary structure is diffuse and
 *   largely local, and it is resistant to top-down revision almost by
 *   construction, since the site of practice is distributed rather than
 *   institutional.
 *
 * KEY AGENTS:
 *   - household_ritual_practitioners: primary agenda-setters (powerless/mobile) — determine actual devotional content
 *   - village_shrine_keepers: local beneficiaries (powerless/mobile) — modest, contingent authority tied to efficacy
 *   - itinerant_healers_and_diviners: mobile beneficiaries (powerless/mobile) — cross-boundary religious specialists
 *   - state_priesthood: excluded institutional claimant (institutional/constrained) — cannot govern this layer
 *   - pharaonic_court: excluded institutional claimant (institutional/constrained) — policy does not reach this substrate
 *   - later_historians_and_egyptologists: analytical observers reconstructing from material remains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18).
domain_priors:suppression_score(divine_legitimacy_substrate__folk_syncretistic_reading, 0.12).
domain_priors:theater_ratio(divine_legitimacy_substrate__folk_syncretistic_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(divine_legitimacy_substrate__folk_syncretistic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(divine_legitimacy_substrate__folk_syncretistic_reading, rope).
narrative_ontology:human_readable(divine_legitimacy_substrate__folk_syncretistic_reading, "Folk Syncretistic Reading of Divine Legitimacy (Household/Village Ritual Practice)").
narrative_ontology:topic_domain(divine_legitimacy_substrate__folk_syncretistic_reading, "Ancient History / Religious Studies / Political Economy of Belief Systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(divine_legitimacy_substrate__folk_syncretistic_reading, '4b2c6120-ec51-49b2-9615-58aff11df9b7').
narrative_ontology:cs_kernel_codification('4b2c6120-ec51-49b2-9615-58aff11df9b7', distributed).
narrative_ontology:cs_authority_grounding('4b2c6120-ec51-49b2-9615-58aff11df9b7', practice).
narrative_ontology:cs_interpretation_layer_present('4b2c6120-ec51-49b2-9615-58aff11df9b7').
narrative_ontology:cs_reading_relation('4b2c6120-ec51-49b2-9615-58aff11df9b7', divine_legitimacy_substrate__amun_polytheistic_reading, coexists_with).
narrative_ontology:cs_reading_relation('4b2c6120-ec51-49b2-9615-58aff11df9b7', divine_legitimacy_substrate__atenist_monotheistic_reading, influences).
narrative_ontology:cs_axiom('4b2c6120-ec51-49b2-9615-58aff11df9b7', foundational, efficacy_grounds_legitimacy_not_office).
narrative_ontology:cs_axiom_status(efficacy_grounds_legitimacy_not_office, holdable).
narrative_ontology:cs_axiom_grounding('4b2c6120-ec51-49b2-9615-58aff11df9b7', efficacy_grounds_legitimacy_not_office, instrumental).
narrative_ontology:cs_axiom('4b2c6120-ec51-49b2-9615-58aff11df9b7', foundational, pragmatic_pluralism_requires_no_doctrinal_reconciliation).
narrative_ontology:cs_axiom_status(pragmatic_pluralism_requires_no_doctrinal_reconciliation, holdable).
narrative_ontology:cs_axiom_grounding('4b2c6120-ec51-49b2-9615-58aff11df9b7', pragmatic_pluralism_requires_no_doctrinal_reconciliation, conventional).
narrative_ontology:cs_reference_frame('4b2c6120-ec51-49b2-9615-58aff11df9b7', household_efficacy_tested_pluralism).
narrative_ontology:cs_drift_state('4b2c6120-ec51-49b2-9615-58aff11df9b7', post_amarna_restoration, gap(stable, minor, false)).
narrative_ontology:cs_created_at('4b2c6120-ec51-49b2-9615-58aff11df9b7', '').
narrative_ontology:cs_kernel_id(divine_legitimacy_substrate__folk_syncretistic_reading, divine_legitimacy_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, household_ritual_practitioners).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, village_shrine_keepers).
narrative_ontology:constraint_beneficiary(divine_legitimacy_substrate__folk_syncretistic_reading, itinerant_healers_and_diviners).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, pragmatic_pluralism_as_legitimate_devotion).
narrative_ontology:constraint_vindicates(divine_legitimacy_substrate__folk_syncretistic_reading, efficacy_over_orthodoxy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordinary householders maintain small shrines, offer to whichever deity's domain matches the immediate need (childbirth, harvest, illness, travel), and pass practices down through family lines. They set the actual content of daily devotion regardless of what temples or palace decree, and can add, drop, or blend deities without asking permission from anyone.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, household_ritual_practitioners, agenda_setter,
    powerless, generational, mobile, local).

% Local part-time custodians of village shrines receive small offerings and social standing in exchange for maintaining rites across a locally-relevant pantheon. Their authority is informal, contingent on perceived efficacy, and evaporates if villagers simply stop coming or start going to a rival shrine or healer instead.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, village_shrine_keepers, beneficiary,
    powerless, generational, mobile, local).
narrative_ontology:stakeholder_secondary_role(divine_legitimacy_substrate__folk_syncretistic_reading, village_shrine_keepers, agenda_setter).

% Traveling specialists invoke whichever deity or spirit is locally credited with healing or oracular power, adapting their practice village to village. They benefit from the pragmatic, non-doctrinal environment because it lets them work across religious boundaries that a centralized cult would police.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, itinerant_healers_and_diviners, beneficiary,
    powerless, biographical, mobile, regional).

% Temple hierarchies attempt to fold folk practice into official theology or dismiss it as unlettered superstition, but have no real mechanism to audit or correct what happens at household altars. They are structurally excluded from the actual site of practice even though they claim jurisdiction over 'true' religion.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, state_priesthood, excluded,
    institutional, civilizational, constrained, national).

% The court issues religious policy — sometimes syncretic, sometimes exclusive — but village practice runs largely independent of royal decree; edicts reshape temple ritual and elite monuments far more easily than they reshape which god a farmer thanks for rain. The court experiences the substrate as something it cannot fully see or govern.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, pharaonic_court, excluded,
    institutional, civilizational, constrained, national).

% Reconstruct folk religious practice from material remains (household shrines, amulets, votive stelae) precisely because it left little textual record compared to state and temple religion. They note the difficulty of assigning this layer a clear beneficiary or victim structure, since it appears to have operated largely outside elite extraction.
narrative_ontology:constraint_stakeholder(divine_legitimacy_substrate__folk_syncretistic_reading, later_historians_and_egyptologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides households and villages with an adaptable, low-cost ritual toolkit for managing uncertainty (illness, childbirth, harvest, travel, death) by drawing pragmatically on whichever deity or local spirit is locally credited with relevant efficacy, without requiring doctrinal consistency or institutional mediation.
% TRANSFER_FUNCTION: Modest, mostly local: small offerings and labor flow to shrine keepers and itinerant specialists in exchange for ritual services; no significant wealth or authority is transferred upward to temple or crown through this layer specifically.
% ABSENT_VOICES: State priesthood and pharaonic court both claim jurisdiction over 'legitimate' religion but are structurally absent from the household/village site of practice; they would object that folk syncretism is theologically incoherent or beneath notice, but they cannot observe or correct it directly, which is part of why the historical record underrepresents this layer relative to elite religion.
% DISAPPEARANCE_RATIONALE: If household/village syncretic practice vanished overnight, elite monuments, temple economies, and royal inscriptions would be untouched, suggesting world_unchanged from the state's vantage. But for the overwhelming majority of the population whose actual devotional life occurred here rather than in state temples, daily life would rearrange substantially — the loss would be invisible to the textual record and total to lived experience, which is exactly why the verdict is contested rather than settled.
% FOUNDING_PROBLEM: Ordinary households and villages needed ways to manage everyday uncertainty and misfortune (illness, birth, crop failure, safe travel) without access to state temple resources, priestly literacy, or royal ritual apparatus — a low-overhead, locally-adaptable devotional practice solved this directly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated primarily by material-archaeological evidence (household shrines, amulets, votive objects) interpreted by later historians and Egyptologists standing entirely outside the folk-practitioner and shrine-keeper population; the state priesthood and pharaonic court, who are the closest thing to a contemporaneous outside observer, largely ignored or dismissed the practice rather than attesting to it, so corroboration comes overwhelmingly from modern analytical reconstruction rather than any ancient elite witness.
narrative_ontology:disappearance_verdict(divine_legitimacy_substrate__folk_syncretistic_reading, contested).
narrative_ontology:founding_problem_status(divine_legitimacy_substrate__folk_syncretistic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(divine_legitimacy_substrate__folk_syncretistic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(divine_legitimacy_substrate__folk_syncretistic_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).
:- end_tests(divine_legitimacy_substrate__folk_syncretistic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18) and essentially flat across the interval because no concentrated party captures rents from folk practice at scale; what changes hands is small, local, and reciprocal (offerings for services rendered), not a rent stream feeding an institution. Suppression is low (0.12) because nothing enforces participation or a particular ritual mix; multiple deities are invoked pragmatically and can be dropped or added without penalty. Theater ratio is modest and slowly rising (0.18 to 0.22) reflecting the ordinary tendency of any inherited ritual form to accumulate some performative residue over generations, but it never becomes dominant because efficacy-testing (does the rite work?) continuously prunes purely theatrical elements. Accessibility collapse is low (0.25) — alternative devotional configurations remain genuinely available and are routinely adopted; resistance is low (0.15) because there is little to resist against, the practice being voluntary and non-coercive at its core.
 *
 * PERSPECTIVAL GAP:
 *   From the household practitioner's seat, this substrate looks like ordinary life-management, not a religious 'system' at all — hence the low resistance and low accessibility collapse. From the state priesthood's or pharaonic court's seat, the same substrate looks like a gap in their jurisdiction: something they claim authority over but cannot see, correct, or tax. The engine should compute divergent seat types here — an unenforced, near-Rope reading from the practitioner seats and something closer to an irritant or non-event from the excluded institutional seats, since institutional actors neither benefit from nor are extracted by this layer in any measurable way.
 *
 * DIRECTIONALITY LOGIC:
 *   Household practitioners and shrine keepers sit near the beneficiary end of directionality: the practice subsidizes their coping capacity and, for shrine keepers, provides modest local standing, at negligible cost to themselves. Itinerant healers benefit similarly through freedom of movement across religious boundaries. There are no declared victims because the reading's own structural claim is that beneficiary structure is genuinely unclear and diffuse — imposing a victim group here would misrepresent the source material's explicit claim that this layer resists the extraction/coordination framing that fits its siblings.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing everyday uncertainty without state-temple resources) remains fully live for the population that relied on it, which is why founding_problem_status is 'live' rather than 'dead' — there is no mandatrophy here to resolve, since the mandate and the function have not diverged. This is precisely why the classification should not collapse into Snare or Tangled Rope: the coordination function is real, ongoing, and does not require active enforcement to persist, distinguishing it sharply from the extraction-laden institutional readings of the same kernel.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    beneficiary_structure_indeterminacy,
    'Does folk syncretistic practice have any identifiable beneficiary structure beyond the diffuse, near-universal population that practices it, or is ''beneficiary'' the wrong frame entirely for a substrate this decentralized?',
    'Comparative archaeological analysis of household shrine investment (labor, materials, offerings) against measurable outcomes (health, agricultural yield perception) to see if any local specialist class captures disproportionate value versus ordinary reciprocal exchange.',
    'If a genuine capturing class is found (e.g., shrine keepers systematically extracting more than services rendered), the constraint would drift toward tangled_rope; if not, the rope/near-mountain reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_indeterminacy, empirical, 'Whether folk practice truly lacks a beneficiary-extraction structure or merely lacks documentation of one.').

omega_variable(
    kernel_framing_choice_household_vs_legitimacy_claim,
    'Is the correct kernel-level framing the household ritual practice itself, or the higher-order legitimacy CLAIM that such practice constitutes valid divine sanction independent of priestly or pharaonic mediation?',
    'Compare classification outcomes under (a) treating the practice as the constraint object versus (b) treating the meta-claim ''household practice is itself legitimating'' as the constraint object; assess whether either framing changes beneficiary declarations or theater_ratio trajectory.',
    'Framing (a), used here, yields a low-extraction near-Rope reading centered on lived practice. Framing (b) would shift the constraint toward an ideological/doctrinal claim contested by priesthood and court, potentially raising suppression and resistance since elite actors do have some interest in denying the meta-claim even while ignoring the practice itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_choice_household_vs_legitimacy_claim, conceptual, 'Whether the constraint object should be the practice or the legitimacy-claim about the practice; this story adopts the practice-level framing.').

omega_variable(
    diffuse_authority_persistence_mechanism,
    'Why does this substrate resist top-down revision by pharaoh or priesthood — is it genuine institutional incapacity to reach the household level, deliberate elite non-interference (tolerance as cheaper than suppression), or active local resistance to elite religious reform attempts (e.g., the Amarna period)?',
    'Cross-reference known reform episodes (Akhenaten''s Atenist program) against archaeological continuity of household polytheistic material culture in the same period; persistence through an active suppression attempt would indicate resistance/incapacity rather than mere elite indifference.',
    'If household practice persisted essentially unchanged through the Amarna reforms, this strongly supports low suppression and high resistance-to-revision as structural facts rather than artifacts of elite disinterest; if it shifted measurably, resistance is weaker than authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_authority_persistence_mechanism, empirical, 'What structurally explains the folk substrate''s resistance to top-down religious revision.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(divine_legitimacy_substrate__folk_syncretistic_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(divi_tr_t0, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(divi_tr_t20, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(divi_tr_t40, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(divi_tr_t60, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement(divi_tr_t80, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 80, 0.21).
narrative_ontology:measurement(divi_tr_t100, divine_legitimacy_substrate__folk_syncretistic_reading, theater_ratio, 100, 0.22).

% Extraction over time
narrative_ontology:measurement(divi_be_t0, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(divi_be_t20, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 20, 0.16).
narrative_ontology:measurement(divi_be_t40, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(divi_be_t60, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 60, 0.18).
narrative_ontology:measurement(divi_be_t80, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 80, 0.18).
narrative_ontology:measurement(divi_be_t100, divine_legitimacy_substrate__folk_syncretistic_reading, base_extractiveness, 100, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(divine_legitimacy_substrate__folk_syncretistic_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(divine_legitimacy_substrate__folk_syncretistic_reading, identity_coordination).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, amun_polytheistic_reading).
narrative_ontology:affects_constraint(divine_legitimacy_substrate__folk_syncretistic_reading, atenist_monotheistic_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'divine legitimacy in ancient Egypt' per the ε-invariance principle: amun_polytheistic_reading (priestly interpretive authority, elite temple economy, moderate-to-high extraction), atenist_monotheistic_reading (exclusive pharaonic revelation, high suppression during active enforcement, contested restoration afterward), and this folk_syncretistic_reading (diffuse household/village practice, low extraction, low suppression, unclear beneficiary structure). Each reading has a distinct ε and distinct stakeholder set; they are linked here rather than merged because measuring 'divine legitimacy' via the household-practice observable versus the priestly-interpretation observable versus the pharaonic-revelation observable yields structurally different constraints, not different views of one constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
