% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Catastrophe Memory Ritual as Intergenerational Trauma Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This story instantiates the trauma-encoding reading of the catastrophe
 *   memory kernel: the claim that a community's mourning ritual functions
 *   primarily as a mechanism for transmitting the psychological alarm-state
 *   of catastrophe survivors to descendants who did not live through the
 *   originating event, in order to preserve collective early-warning capacity
 *   across generations. As the founding survivor generation recedes, the
 *   ritual's transmission function intensifies rather than fades — the
 *   mechanism must work harder to encode vigilance in generations with no
 *   direct memory, which is read here as a rising extractiveness trajectory
 *   over the interval. This reading shares its kernel with three siblings —
 *   symbol_continuity_reading (identity/continuity function),
 *   survival_competence_reading (adaptive-skill transmission), and
 *   boundary_maintenance_reading (in-group/out-group enforcement) — each of
 *   which is authored as a separate constraint story with its own ε and
 *   stakeholder structure per the ε-invariance principle. The four readings
 *   are not competing measurements of one constraint; they are four distinct
 *   structural claims about what the same ritual practice does.
 *
 * KEY AGENTS:
 *   - elder_ritual_custodians: administer and design the ritual, identity fused with custodial role (organized/identity_locked)
 *   - descendant_generations: bear the transmitted trauma burden across biographical and generational timescales (powerless/identity_locked)
 *   - children_inducted_early: bear early, non-consensual, developmentally premature exposure (powerless/trapped)
 *   - collective_threat_vigilance_function: the non-agent coordination good the ritual is claimed to produce
 *   - assimilationist_descendants: would reduce ritual intensity but are structurally excluded from ritual-design conversation
 *   - clinical_and_academic_observers: analytical seat studying transmission costs and benefits from outside the community
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.62).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.48).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Catastrophe Memory Ritual as Intergenerational Trauma Encoding").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '7787c868-55d9-4f2d-ac48-11904f2d5dbd').
narrative_ontology:cs_kernel_codification('7787c868-55d9-4f2d-ac48-11904f2d5dbd', distributed).
narrative_ontology:cs_authority_grounding('7787c868-55d9-4f2d-ac48-11904f2d5dbd', practice).
narrative_ontology:cs_interpretation_layer_present('7787c868-55d9-4f2d-ac48-11904f2d5dbd').
narrative_ontology:cs_reading_relation('7787c868-55d9-4f2d-ac48-11904f2d5dbd', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('7787c868-55d9-4f2d-ac48-11904f2d5dbd', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('7787c868-55d9-4f2d-ac48-11904f2d5dbd', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('7787c868-55d9-4f2d-ac48-11904f2d5dbd', foundational, trauma_transmission_is_the_primary_mechanism).
narrative_ontology:cs_axiom_status(trauma_transmission_is_the_primary_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('7787c868-55d9-4f2d-ac48-11904f2d5dbd', trauma_transmission_is_the_primary_mechanism, empirically_contingent).
narrative_ontology:cs_axiom('7787c868-55d9-4f2d-ac48-11904f2d5dbd', secondary, descendant_psychological_cost_is_the_operative_currency_of_transmission).
narrative_ontology:cs_axiom_status(descendant_psychological_cost_is_the_operative_currency_of_transmission, holdable).
narrative_ontology:cs_axiom_grounding('7787c868-55d9-4f2d-ac48-11904f2d5dbd', descendant_psychological_cost_is_the_operative_currency_of_transmission, empirically_contingent).
narrative_ontology:cs_reference_frame('7787c868-55d9-4f2d-ac48-11904f2d5dbd', survivor_generation_direct_witness_transmission).
narrative_ontology:cs_drift_state('7787c868-55d9-4f2d-ac48-11904f2d5dbd', third_and_fourth_generation_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7787c868-55d9-4f2d-ac48-11904f2d5dbd', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance_function).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, children_inducted_early).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_recurrence_is_real_risk).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and administer the mourning liturgy, decide which catastrophe narratives are ritually rehearsed, and set the age and manner at which children are inducted into the full weight of the memory. Their authority and social standing derive substantially from being custodians of the community's most sacred grief; they experience the ritual as sacred duty, not extraction, and cannot easily separate their identity from the transmission role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__trauma_encoding_reading, elder_ritual_custodians, beneficiary).

% Not a person but the standing capacity the ritual is claimed to produce: a population primed to recognize early signs of persecution, expulsion, or genocide and respond faster than an unwarned population would. This capacity is the coordination good the ritual is said to purchase, financed by the trauma costs imposed on those who carry it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance_function, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_kernel__trauma_encoding_reading, collective_threat_vigilance_function).

% Inherit the full emotional weight of ancestral catastrophe without having chosen it and often before they have the psychological resources to process it. They carry hypervigilance, anticipatory grief, and identity organized around persecution long after the immediate threat that produced the original trauma has receded in their actual environment. Leaving the community can mean losing family, belonging, and the interpretive frame that makes sense of inherited anxiety — exit is available in principle but costs the self that would have to do the leaving.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendant_generations, payer,
    powerless, generational, identity_locked, national).

% Are brought into the ritual's full narrative content — survivor testimony, atrocity detail, commemorative reenactment — before they have the developmental capacity to metabolize it, on the premise that early induction produces durable vigilance. They have no say in the timing or intensity of exposure and no exit from the household or community that administers it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, children_inducted_early, payer,
    powerless, biographical, trapped, local).

% Would prefer to reduce ritual intensity, marry out, or raise children with less catastrophe-centered identity formation. Their preference is rarely solicited in ritual design; voicing it inside the community risks being read as betrayal of the dead, which suppresses the objection before it is heard.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, assimilationist_descendants, excluded,
    moderate, biographical, constrained, national).

% Trauma researchers and scholars of collective memory who study epigenetic and psychosocial transmission of catastrophe memory across generations, document the costs (anxiety disorders, hypervigilance, complicated grief) and benefits (group cohesion, threat responsiveness) without being party to the ritual's administration.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, clinical_and_academic_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the ritual's real coordination problem is early threat detection: a population that has suffered catastrophic persecution faces a recurring risk that early warning signs will be missed or normalized by a generation that did not live through the original catastrophe. Ritualized transmission of trauma is read as a mechanism for keeping vigilance alive past the lifespan of direct survivors.
% TRANSFER_FUNCTION: The arrangement transfers psychological burden — hypervigilance, anticipatory grief, trauma-organized identity — from the generation that experienced catastrophe to generations that did not, in exchange for (claimed) improved collective capacity to recognize recurrence. The cost is paid in the currency of descendants' mental health and developmental experience; the benefit is collected as a diffuse, hard-to-verify improvement in group threat-detection.
% ABSENT_VOICES: Descendants who experience the ritual primarily as harm — those diagnosed with trauma-adjacent conditions traceable to early ritual induction, or those who have quietly exited the community — rarely appear in the ritual's own account of itself. Their testimony would complicate the vigilance-benefit claim but is structurally hard to collect because raising it inside the community reads as disloyalty to survivors.
% DISAPPEARANCE_RATIONALE: Custodians and much of the community would say the world rearranges catastrophically — vigilance erodes, the community becomes vulnerable to a repeat of history because no one recognizes the warning signs. Clinical observers and assimilationist descendants would say the diffuse threat-detection benefit is empirically unverified and the removal of high-intensity early induction would mainly reduce measurable trauma symptoms in descendants without any demonstrated increase in actual vulnerability to persecution. The disagreement is not resolvable from within either seat alone.
% FOUNDING_PROBLEM: A catastrophic historical persecution or atrocity occurred; the immediate community needed both to grieve and to ensure the memory of danger signs would not die with the survivor generation, so that recurrence could be recognized and resisted earlier next time.
% FOUNDING_PROBLEM_CORROBORATION: Elder custodians and much of the survivor-descended community attest the threat is still live, citing ongoing antisemitism, genocide denial, or comparable persecution elsewhere as proof the vigilance function remains necessary. Clinical and academic observers outside the beneficiary community — trauma researchers studying intergenerational transmission — corroborate that a real founding trauma occurred but find no reliable evidence that high-intensity ritual induction of children improves actual threat-detection outcomes relative to lower-intensity transmission; their corroboration supports the historical founding problem while contesting whether the current intensity of the mechanism remains proportionate to it.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, contested).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-to-high (0.62 at interval end) because real psychological costs are imposed on non-consenting descendants and the benefit collected (improved threat detection) is diffuse and empirically unverified rather than concretely measurable. Suppression is moderate (0.48): objection is discouraged through loyalty and grief norms rather than formal coercion, and exit exists nominally but at high identity cost. Theater ratio is low-to-moderate and rising (0.10 to 0.28): as direct survivor memory fades, an increasing share of ritual activity becomes performative reenactment disconnected from any living witness, which is exactly the drift the framework's theater-ratio metric is built to catch. Accessibility collapse is moderate (0.40) — genuine alternative modes of memorialization (lower-intensity, adult-onset, opt-in) exist and are used by some diaspora communities, so alternatives have not fully collapsed. Resistance is moderate-high (0.55), reflecting assimilationist and clinical pushback against high-intensity induction of children specifically.
 *
 * PERSPECTIVAL GAP:
 *   From the elder-custodian seat, the arrangement is sacred obligation to the dead and to future safety, not extraction — the vigilance benefit is real and the cost is the price of collective survival. From the descendant and inducted-child seats, the same structural arrangement operates as an unchosen inheritance of pain imposed for a benefit they never agreed to underwrite and cannot verify. The engine computes these as structurally different per-seat classifications from the same authored data; this divergence is the phenomenon, not an error to reconcile.
 *
 * DIRECTIONALITY LOGIC:
 *   Elder custodians sit near the beneficiary end: they administer the mechanism and derive social standing, meaning, and communal position from the custodial role, even though they do not extract material rent. Descendant generations and inducted children sit near the target end: they bear the psychological transfer with identity-locked or fully trapped exit options respectively — children especially cannot exit the household administering their induction. Assimilationist descendants have somewhat more mobility (moderate power, constrained exit) but remain structurally excluded from ritual-design conversations, which suppresses their preference before it registers as resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — surviving a specific historical catastrophe and needing to recognize warning signs of its recurrence — was real and remains partially live given ongoing documented persecution elsewhere. But the founding_problem_status is authored contested rather than simply live: outside corroboration (clinical/academic) supports the historical trauma but not the claim that current-intensity, early-childhood induction is proportionate to present threat levels. This is precisely the tangled-rope signature: a genuine coordination function (threat vigilance) coexists with an extraction structure (trauma imposed on non-consenting descendants) that requires active enforcement (loyalty/grief norms suppressing internal objection) to persist at its current intensity. Classifying this as pure snare would erase the real historical grounding and documented value of collective memory; classifying it as pure rope would erase the measurable psychological cost imposed on children who did not choose it. Tangled rope holds both facts without collapsing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vigilance_benefit_verifiability,
    'Does high-intensity ritual transmission of catastrophe trauma to descendants actually produce measurably better early-warning/threat-recognition outcomes, or is the vigilance-benefit claim unfalsifiable and self-confirming within the community that holds it?',
    'Comparative studies of communities with varying ritual-transmission intensity, tracking documented instances of early threat recognition and response against ritual intensity and induction age, controlling for community size, historical persecution severity, and geographic dispersion.',
    'If the vigilance benefit is empirically supported at current intensity, the coordination function is real and substantial, supporting a tangled-rope reading closer to rope. If unsupported or achievable at much lower induction intensity, the arrangement looks more purely extractive of descendant welfare with the coordination story functioning mainly as justification — pushing toward snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vigilance_benefit_verifiability, empirical, 'Whether the claimed threat-vigilance benefit of high-intensity trauma transmission is empirically real or self-justifying.').

omega_variable(
    kernel_reading_boundary_location,
    'Where does the trauma-encoding reading''s boundary sit relative to the survival_competence_reading — is transmitted hypervigilance the same phenomenon as transmitted adaptive survival skill, differently described, or are they genuinely separable mechanisms within the same ritual practice?',
    'Ethnographic and psychological analysis distinguishing ritual content that transmits specific actionable survival strategies (recognizing early warning signs, escape planning, community mutual-aid networks) from content that transmits undifferentiated emotional alarm without corresponding actionable competence.',
    'If the two are empirically inseparable, the trauma_encoding_reading and survival_competence_reading describe the same underlying mechanism from different evaluative stances, which would argue for treating them as one constraint rather than two per the ε-invariance principle. If separable, the two-reading decomposition is structurally justified as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary_location, conceptual, 'Whether trauma-encoding and survival-competence readings pick out the same underlying mechanism or genuinely distinct ones.').

omega_variable(
    identity_lock_reversibility,
    'Is the identity-lock experienced by descendant generations and elder custodians reversible through therapeutic intervention or generational distance, or does it represent a permanent structural feature of communities formed by catastrophic persecution?',
    'Longitudinal study of descendant cohorts that received trauma-informed therapeutic support alongside ritual participation versus cohorts that received ritual participation alone, tracking whether identity-lock (measured via willingness to modify or exit ritual practice) changes over time or with intervention.',
    'If identity-lock is reversible with support, the extractiveness measured here reflects a remediable feature of current practice rather than an intrinsic property of the coordination mechanism, suggesting achievable lower-cost versions of the same vigilance function. If irreversible, the current cost structure may be closer to an inherent feature of any catastrophe-memory transmission mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_reversibility, empirical, 'Whether descendants'' identity-locked exit options are a fixed feature of trauma transmission or a remediable consequence of current ritual design.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 10, 0.13).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.17).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 30, 0.2).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.23).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 10, 0.46).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.51).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 50, 0.6).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.62).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_kernel__trauma_encoding_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__trauma_encoding_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This story is one of four sibling constraints decomposed from the single natural-language concept 'catastrophe memory ritual' per the ε-invariance principle. Each sibling reading (trauma_encoding, symbol_continuity, survival_competence, boundary_maintenance) attributes a structurally distinct function, beneficiary set, and victim set to the same observable ritual practice, and each carries its own ε rather than sharing one averaged value. This reading's ε (0.62) is authored specifically for the trauma-transmission function and victim-of-psychological-burden framing; the sibling stories will carry different ε values reflecting their distinct claims (continuity preservation, skill transmission, boundary enforcement respectively).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
