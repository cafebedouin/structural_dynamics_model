% ============================================================================
% CONSTRAINT STORY: honor_satisfaction_mechanism__decline_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_honor_satisfaction_mechanism__decline_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: honor_satisfaction_mechanism__decline_reading
 *   human_readable: Honor Satisfaction Mechanism — Decline Reading
 *   domain: historical_sociology/legal_history/normative_systems
 *
 * SUMMARY:
 *   The honor satisfaction mechanism — dueling as the institutionalized way
 *   gentlemen resolved status injuries — persisted from the early 18th
 *   century to the late 19th century at declining frequency until it became a
 *   fringe practice. This decline_reading treats the constraint as a single
 *   mechanism that weakened gradually: the same code duello operated
 *   throughout, but enforcement (social pressure to fight), participation
 *   rates, and lethal outcomes all dropped. Epsilon falls because fewer men
 *   were compelled to risk death, and the social cost of refusal declined as
 *   legal alternatives emerged. The mechanism did not become cognitively
 *   unthinkable (contraction_reading) nor fracture into distinct mechanisms
 *   (composite_reading) — it simply thinned out.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(honor_satisfaction_mechanism__decline_reading, 0.45).
domain_priors:suppression_score(honor_satisfaction_mechanism__decline_reading, 0.55).
domain_priors:theater_ratio(honor_satisfaction_mechanism__decline_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(honor_satisfaction_mechanism__decline_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(honor_satisfaction_mechanism__decline_reading, tangled_rope).
narrative_ontology:human_readable(honor_satisfaction_mechanism__decline_reading, "Honor Satisfaction Mechanism — Decline Reading").
narrative_ontology:topic_domain(honor_satisfaction_mechanism__decline_reading, "historical_sociology/legal_history/normative_systems").

domain_priors:requires_active_enforcement(honor_satisfaction_mechanism__decline_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(honor_satisfaction_mechanism__decline_reading, 'e44f91a5-1313-48d0-9ada-7c81e702c239').
narrative_ontology:cs_kernel_codification('e44f91a5-1313-48d0-9ada-7c81e702c239', distributed).
narrative_ontology:cs_authority_grounding('e44f91a5-1313-48d0-9ada-7c81e702c239', practice).
narrative_ontology:cs_interpretation_layer_present('e44f91a5-1313-48d0-9ada-7c81e702c239').
narrative_ontology:cs_reading_relation('e44f91a5-1313-48d0-9ada-7c81e702c239', honor_satisfaction_mechanism__contraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('e44f91a5-1313-48d0-9ada-7c81e702c239', honor_satisfaction_mechanism__composite_reading, influences).
narrative_ontology:cs_axiom('e44f91a5-1313-48d0-9ada-7c81e702c239', foundational, honor_satisfaction_requires_violent_risk).
narrative_ontology:cs_axiom_status(honor_satisfaction_requires_violent_risk, holdable).
narrative_ontology:cs_axiom_grounding('e44f91a5-1313-48d0-9ada-7c81e702c239', honor_satisfaction_requires_violent_risk, deontological).
narrative_ontology:cs_axiom('e44f91a5-1313-48d0-9ada-7c81e702c239', secondary, state_law_cannot_substitute_for_personal_risk).
narrative_ontology:cs_axiom_status(state_law_cannot_substitute_for_personal_risk, holdable).
narrative_ontology:cs_axiom_grounding('e44f91a5-1313-48d0-9ada-7c81e702c239', state_law_cannot_substitute_for_personal_risk, deontological).
narrative_ontology:cs_reference_frame('e44f91a5-1313-48d0-9ada-7c81e702c239', aristocratic_honor_code).
narrative_ontology:cs_drift_state('e44f91a5-1313-48d0-9ada-7c81e702c239', bourgeois_legal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('e44f91a5-1313-48d0-9ada-7c81e702c239', '').
narrative_ontology:cs_kernel_id(honor_satisfaction_mechanism__decline_reading, honor_satisfaction_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, aristocratic_elites).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, honor_claimants).
narrative_ontology:constraint_beneficiary(honor_satisfaction_mechanism__decline_reading, military_officer_corps).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, duel_participants).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, seconds).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, families_of_duelists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(honor_satisfaction_mechanism__decline_reading, military_officer_corps).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, honor_as_social_capital).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, violence_as_dispute_resolution).
narrative_ontology:constraint_vindicates(honor_satisfaction_mechanism__decline_reading, personal_risk_as_honest_signal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authored and maintained the codes duello; used dueling to police membership in the honor group and settle disputes without state interference. Their status depended on willingness to duel, but they controlled the rules and could often avoid fighting through apologies or seconds' negotiations. Exit meant losing caste — they were identity_locked to the practice until alternative status markers (wealth, state service) matured.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, aristocratic_elites, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, aristocratic_elites, beneficiary).

% Men who invoked the code to demand satisfaction for insults. They gained leverage — the threat of a duel forced apologies or public submissions without shots fired. But they were also bound: refusing a legitimate challenge meant social death. As courts offered defamation remedies, their exit options improved from constrained toward mobile.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, honor_claimants, beneficiary,
    powerful, biographical, constrained, national).

% Officers were the most frequent duelists; the code was enforced by regimental honor councils. Duels demonstrated courage (career currency) but killed promising officers. The corps benefited from the cohesion the ritual produced but paid in blood. As state law criminalized dueling, they became payers — caught between regimental honor and military law.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, military_officer_corps, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, military_officer_corps, payer).

% The principals who faced pistols or swords. Once challenged, refusal meant permanent dishonor; acceptance meant ~15-25% mortality per encounter (higher for swords). No exit once the process started — seconds managed the mechanics, but the participant's body bore the cost. Declining frequency meant fewer faced this trap over time.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, duel_participants, payer,
    moderate, immediate, trapped, local).

% Friends or peers tasked with arranging terms, loading pistols, and sometimes fighting in place of the principal. They risked prosecution as accessories and carried moral weight for enabling death. Their role was identity-defining — to refuse was to betray friendship and honor. As dueling faded, the role atrophied but the identity binder persisted in memoir literature.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, seconds, payer,
    moderate, immediate, identity_locked, local).

% Widows, children, parents who bore the economic and social costs of a death or disability. Had no voice in the challenge, no standing in the code, no exit from the consequences. Their victimization was structural — the mechanism externalized its costs onto kin who could not participate in the honor economy.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, families_of_duelists, payer,
    powerless, generational, trapped, local).

% Courts and legislatures that criminalized dueling (e.g., 1819 UK statute, 1839 French law, Prussian 1850s edicts). Initially they suppressed the practice; later they provided the substitute mechanism (defamation law, libel courts) that made dueling obsolete. Their role shifted from external suppressor to internal替代 — they did not just ban, they built the exit ramp.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, legal_authorities, observer,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(honor_satisfaction_mechanism__decline_reading, legal_authorities, agenda_setter).

% Lawyers, doctors, merchants, journalists who needed reputation but were barred from the aristocratic code or chose not to enter it. They developed parallel mechanisms — press duels, literary feuds, professional censure — and lobbied for legal defamation remedies. Their exclusion was not passive; they actively built the alternative that displaced the old mechanism.
narrative_ontology:constraint_stakeholder(honor_satisfaction_mechanism__decline_reading, bourgeois_professionals, excluded,
    organized, biographical, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolved honor disputes among armed elites through a ritualized, bounded violence that prevented unbounded blood feuds and provided a clear status hierarchy — who would risk death, who would apologize, who would negotiate. The code duello supplied procedural legitimacy: seconds, witnesses, agreed weapons, surgeons — making the violence legible and containable.
% TRANSFER_FUNCTION: Moved the risk of death and injury from the honor claimant (who initiated the challenge) to the duel participant (who bore the physical cost). Moved social honor — the willingness to fight became the honest signal of status — from those who could avoid the field to those who could not. Transferred dispute resolution authority from kinship/feud logic to a pseudo-legal ritual with its own jurisprudence (the code).
% ABSENT_VOICES: Women (wives, mothers, sisters) who bore widowhood and destitution but had no standing to object; commoners who were sometimes killed as bystanders or forced into service as seconds; religious authorities (Catholic and Protestant) who condemned dueling as suicide/murder but were overruled by military and aristocratic power; early jurists who argued for state monopoly on violence but lacked enforcement capacity until the 19th century.
% DISAPPEARANCE_RATIONALE: When dueling vanished (last fatal duel in France 1967, Germany 1850s, UK 1840s, US 1850s), the honor disputes did not disappear — they migrated to courts (libel, defamation), press (public shaming), professional bodies (censure), and parliamentary privilege. The world rearranged: the coordination function (bounded dispute resolution) was absorbed by legal systems; the extraction function (risk of death) was eliminated; the status signal (willingness to fight) was replaced by institutional credentials and public reputation.
% FOUNDING_PROBLEM: How to resolve status disputes among armed, proud men who recognized no superior judge — without triggering endless private warfare that destroyed the social order the elites depended on. The code duello was the answer: a self-administered, ritualized violence that substituted one death for a hundred.
% FOUNDING_PROBLEM_CORROBORATION: Norbert Elias (The Civilizing Process) documents the state monopoly on violence displacing private violence; Stephen Banks (A Polite Exchange of Bullets) shows legal defamation replacing dueling in England; Robert Nye (Masculinity and Male Codes of Honor) demonstrates the founding problem (private violence among elites) was solved by state law and bourgeois reputation markets — corroborated by historians outside the aristocratic beneficiary class.
narrative_ontology:disappearance_verdict(honor_satisfaction_mechanism__decline_reading, world_rearranges).
narrative_ontology:founding_problem_status(honor_satisfaction_mechanism__decline_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(honor_satisfaction_mechanism__decline_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(honor_satisfaction_mechanism__decline_reading, 'none', 1).
narrative_ontology:epsilon_provenance(honor_satisfaction_mechanism__decline_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(honor_satisfaction_mechanism__decline_reading_tests).
:- end_tests(honor_satisfaction_mechanism__decline_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness starts high (0.78) because the mechanism extracted life and limb from participants who had no exit; it falls to 0.12 by 1900 as legal defamation and professional censure provided exits. Suppression mirrors this: social death for refusers was the enforcement engine, and its power waned as honor became decoupled from violence. Theater ratio rises from 0.18 to 0.85 — early duels were earnest (real risk, real resolution); late duels were often performative (deloping, negotiated apologies, bloodless 'satisfaction') maintaining the ritual after its function migrated to courts. Accessibility collapse is moderate (0.42) because alternatives (courts, press, apology) existed throughout but were imperfect substitutes for elite men. Resistance rises (0.58) as the mechanism lost legitimacy — more men refused, more authorities prosecuted, more publics condemned.
 *
 * PERSPECTIVAL GAP:
 *   The engine should compute sharp seat divergence: from the aristocrat's seat this was a rope (coordination with manageable cost); from the participant's seat a snare (extraction with no exit); from the officer's seat a tangled rope (cohesion bought with blood); from the legal authority's seat a scaffold (temporary support for order until law matured). The claim (tangled_rope) reflects the system-level view — genuine coordination + asymmetric extraction + active enforcement — but the per-seat types will differ.
 *
 * DIRECTIONALITY LOGIC:
 *   Aristocratic elites and honor claimants are structural beneficiaries (d ~ 0.15-0.25) — they controlled the code, gained status from willingness to fight, and could often avoid the field. Military officers sit near symmetric (d ~ 0.45) — they benefited from the cohesion but paid disproportionately in deaths. Duel participants, seconds, and families are targets (d ~ 0.85-0.95) — trapped or identity_locked, bearing mortality and ruin. Legal authorities shift from suppressor (d ~ 0.7 early) to agenda_setter of the replacement (d ~ 0.1 late) — their role transformation is the structural story. Bourgeois professionals are excluded but mobile (d ~ 0.3) — they built the exit ramp.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (private violence among elites) is dead — state law solved it. The mechanism persisted 50-100 years past its functional expiry because the honor identity binder (identity_locked seconds, aristocratic self-concept) created inertial maintenance. This is mandatrophy: the mandate (resolve honor disputes) was fulfilled by courts, but the arrangement (dueling) lingered as theater. The theater_ratio trajectory (0.18→0.85) is the mandatrophy signature — function atrophied, performance remained.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Was the honor satisfaction mechanism''s coordination function (preventing feuds) genuine and irreducible, or was the violence always the point — extraction dressed as order?',
    'Counterfactual: in regions/periods where dueling was suppressed early (e.g., Massachusetts 1720s, Prussia 1790s), did elite feuding increase? If not, the coordination claim is weak; if yes, the mechanism did real work.',
    'If coordination was genuine, the mechanism was a tangled rope (coordination + extraction); if coordination was cover, it was a snare. The decline_reading''s claimed_type depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Whether the constraint''s coordination function was real or rhetorical.').

omega_variable(
    decline_driver_state_vs_culture,
    'Did dueling decline primarily because state enforcement (criminalization, prosecution) raised the cost, or because cultural honor codes internally shifted (bourgeois reputation replaced aristocratic violence)?',
    'Compare jurisdictions: France (early criminalization, persistent dueling) vs England (later criminalization, earlier cultural shift) vs US South (weak state, persistent culture) vs US North (strong state, rapid shift). Disentangle legal vs cultural drivers.',
    'If state-driven, the constraint''s suppression metric reflects external force; if culture-driven, suppression reflects internalized norm change — affecting how the engine models the decline trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decline_driver_state_vs_culture, conceptual, 'Whether the constraint''s weakening was pushed from outside or rotted from within.').

omega_variable(
    kernel_reading_relations,
    'Does the decline_reading foreclose, coexist with, or influence the contraction_reading and composite_reading of the honor_satisfaction_mechanism kernel?',
    'Structural analysis: decline_reading says ''frequency dropped but concept remained available''; contraction_reading says ''concept became unavailable.'' These can coexist (different social strata experienced different cognitive availability) but cannot both be the whole story. Composite_reading says ''multiple mechanisms'' — decline_reading''s single-mechanism arc influences but does not foreclose it.',
    'Determines reading_relations in cs_structure. Coexistence with contraction_reading means both were live positions; influence on composite_reading means the frequency data constrain the multi-mechanism model.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relations, conceptual, 'Structural relationship between this reading and its sibling readings of the same kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Was the suppression that enforced dueling structural (legal penalties for refusal, regimental cashiering) or internalized (the duelist genuinely believed death preferable to dishonor)?',
    'Post-exit trajectories: officers who refused duels and survived socially (e.g., Wellington''s 1829 duel refusal) vs those destroyed. Memoir evidence on whether seconds/principals felt compelled or chose. If internalized, suppression persists after legal barriers fall.',
    'If internalized, the constraint''s effective suppression is higher than legal measures suggest — the target carried the suppression internally. This affects the decline_reading''s suppression trajectory: internalized suppression decays slower than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in the honor mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(honor_satisfaction_mechanism__decline_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hsm_decline_tr_t1700, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1700, 0.18).
narrative_ontology:measurement(hsm_decline_tr_t1740, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1740, 0.22).
narrative_ontology:measurement(hsm_decline_tr_t1780, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1780, 0.35).
narrative_ontology:measurement(hsm_decline_tr_t1820, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1820, 0.52).
narrative_ontology:measurement(hsm_decline_tr_t1860, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1860, 0.71).
narrative_ontology:measurement(hsm_decline_tr_t1900, honor_satisfaction_mechanism__decline_reading, theater_ratio, 1900, 0.85).

% Extraction over time
narrative_ontology:measurement(hsm_decline_be_t1700, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1700, 0.78).
narrative_ontology:measurement(hsm_decline_be_t1740, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1740, 0.72).
narrative_ontology:measurement(hsm_decline_be_t1780, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1780, 0.65).
narrative_ontology:measurement(hsm_decline_be_t1820, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1820, 0.48).
narrative_ontology:measurement(hsm_decline_be_t1860, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1860, 0.28).
narrative_ontology:measurement(hsm_decline_be_t1900, honor_satisfaction_mechanism__decline_reading, base_extractiveness, 1900, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(hsm_decline_su_t1700, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1700, 0.75).
narrative_ontology:measurement(hsm_decline_su_t1740, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1740, 0.7).
narrative_ontology:measurement(hsm_decline_su_t1780, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1780, 0.62).
narrative_ontology:measurement(hsm_decline_su_t1820, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1820, 0.48).
narrative_ontology:measurement(hsm_decline_su_t1860, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1860, 0.35).
narrative_ontology:measurement(hsm_decline_su_t1900, honor_satisfaction_mechanism__decline_reading, suppression_requirement, 1900, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(honor_satisfaction_mechanism__decline_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(honor_satisfaction_mechanism__decline_reading, 0.08).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, legal_dueling_prohibition).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, defamation_law_emergence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, state_monopoly_violence).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, professional_censure_systems).
narrative_ontology:affects_constraint(honor_satisfaction_mechanism__decline_reading, parliamentary_privilege_evolution).

% DUAL FORMULATION NOTE:
% Part of the honor_satisfaction_mechanism constraint family with contraction_reading and composite_reading. This reading emphasizes gradual frequency decline and epsilon drop; contraction_reading emphasizes cognitive unthinkability; composite_reading emphasizes mechanism plurality. All three share the kernel 'honor injuries require personal risk to satisfy.'

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, institutional, 0.15).
constraint_indexing:directionality_override(honor_satisfaction_mechanism__decline_reading, organized, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
