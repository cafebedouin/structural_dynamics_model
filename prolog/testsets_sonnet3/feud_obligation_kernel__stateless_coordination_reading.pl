% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Mechanism
 *   domain: legal_anthropology/comparative_political_systems
 *
 * SUMMARY:
 *   This story authors the stateless-coordination reading of the feud
 *   obligation kernel: the claim that blood-feud liability functions as a
 *   genuine, low-overhead coordination mechanism substituting for absent
 *   centralized justice, with wergild as a coexisting, non-suppressed
 *   alternative. This reading treats feud participants as recipients of a
 *   real good (credible deterrence, a recognized channel for redress) and
 *   treats defection from feud duty, not participation in it, as the locus of
 *   cost. The two sibling readings of this same kernel —
 *   extraction_cycle_reading (feud as destructive rent-extraction preventing
 *   consolidation) and christianized_pacification_reading (feud as usurpation
 *   of divinely/institutionally reserved violence authority) — are NOT
 *   represented here; they are separate constraint files with their own ε,
 *   their own beneficiary/victim sets, and their own claimed types, linked
 *   via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - kin_group_members: primary beneficiaries (organized/identity_locked) — receive collective protection
 *   - aggrieved_lineages: beneficiaries (organized/constrained) — gain a redress channel
 *   - feud_defectors: victims of this reading (powerless/trapped) — pay in honor and expulsion
 *   - outcast_kin: victims (powerless/trapped) — excluded from all protection
 *   - wergild_arbiters: agenda-setters administering the compensation alternative
 *   - comparative_legal_historians: analytical observers weighing coordination vs. extraction evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.32).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.28).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Mechanism").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'd0717068-e7cd-428c-9d22-36e8dc057bce').
narrative_ontology:cs_kernel_codification('d0717068-e7cd-428c-9d22-36e8dc057bce', distributed).
narrative_ontology:cs_authority_grounding('d0717068-e7cd-428c-9d22-36e8dc057bce', practice).
narrative_ontology:cs_interpretation_layer_present('d0717068-e7cd-428c-9d22-36e8dc057bce').
narrative_ontology:cs_reading_relation('d0717068-e7cd-428c-9d22-36e8dc057bce', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0717068-e7cd-428c-9d22-36e8dc057bce', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('d0717068-e7cd-428c-9d22-36e8dc057bce', foundational, kin_liability_produces_genuine_deterrence).
narrative_ontology:cs_axiom_status(kin_liability_produces_genuine_deterrence, holdable).
narrative_ontology:cs_axiom_grounding('d0717068-e7cd-428c-9d22-36e8dc057bce', kin_liability_produces_genuine_deterrence, empirically_contingent).
narrative_ontology:cs_axiom('d0717068-e7cd-428c-9d22-36e8dc057bce', secondary, wergild_constitutes_open_nonsuppressed_alternative).
narrative_ontology:cs_axiom_status(wergild_constitutes_open_nonsuppressed_alternative, holdable).
narrative_ontology:cs_axiom_grounding('d0717068-e7cd-428c-9d22-36e8dc057bce', wergild_constitutes_open_nonsuppressed_alternative, empirically_contingent).
narrative_ontology:cs_reference_frame('d0717068-e7cd-428c-9d22-36e8dc057bce', kin_reciprocal_liability_norm).
narrative_ontology:cs_drift_state('d0717068-e7cd-428c-9d22-36e8dc057bce', post_state_consolidation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0717068-e7cd-428c-9d22-36e8dc057bce', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_group_members).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, aggrieved_lineages).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, local_communities_seeking_deterrence).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_defectors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, outcast_kin).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, self_help_justice_viability).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, kinship_based_deterrence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Belong to a kin network whose collective willingness to pursue vengeance or accept wergild deters attacks on any member. They receive protection and standing in exchange for standing ready to answer a killing with counter-violence or negotiated compensation. Their exit from the obligation would mean exit from the kin group's protection entirely.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_group_members, beneficiary,
    organized, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, kin_group_members, agenda_setter).

% Have suffered a killing or grave injury to a member. The feud obligation gives them a recognized, socially legitimate path to redress — either through retaliation or through wergild negotiation — where no centralized court exists to hear their claim. Without the obligation they would have no recourse at all.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, aggrieved_lineages, beneficiary,
    organized, biographical, constrained, local).

% Kin members who refuse to participate in the retaliatory or compensatory obligation when called upon. They lose honor standing, may be publicly shamed, and in severe cases are expelled from the kin group's protection — leaving them exposed to exactly the violence the group exists to deter.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_defectors, payer,
    powerless, biographical, trapped, local).

% Former kin-group members expelled for refusing feud duties or for unresolved disputes within the lineage. They exist outside any protective network, unable to invoke the obligation on their own behalf and vulnerable to violence with no recognized party to answer for them.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, outcast_kin, payer,
    powerless, biographical, trapped, local).

% Villages and settlements without access to a distant or weak central authority rely on the general knowledge that killings will be answered to suppress casual violence. The credible threat of feud, even when not invoked, lowers the background rate of homicide and theft.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, local_communities_seeking_deterrence, beneficiary,
    moderate, generational, constrained, regional).

% Elders, chieftains, or respected intermediaries who negotiate compensation payments as an alternative to actual violence. They administer the coexisting wergild schedule, which the feud obligation makes credible by supplying the threat that compensation substitutes for. Their standing depends on both sides accepting the settlement as final.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_arbiters, agenda_setter,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, wergild_arbiters, observer).

% Study feud systems (Icelandic, Albanian kanun, Germanic, Corsican) as instances of self-enforcing order without centralized coercive capacity. They assess whether the system functioned as genuine coordination or as a low-level extraction cycle, informed by comparative outcomes across regions.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of a state capable of investigating killings, adjudicating guilt, and enforcing judgment, the feud obligation supplies a credible, socially known threat that any killing will be answered — either in kind or through negotiated compensation — which deters casual violence and gives aggrieved parties a recognized path to redress.
% TRANSFER_FUNCTION: Moves the burden of enforcement from a (nonexistent) central authority onto the kin group itself: kin members commit collective liability (a share of retaliatory risk or compensation payment) in exchange for a share of collective protection. Compensation, when chosen, moves wergild payments from the offending lineage to the aggrieved lineage.
% ABSENT_VOICES: Individuals who dissent from feud logic on ethical grounds (Christian clergy pressing for peace, women bearing indirect costs of cyclical violence in some feud traditions) are largely outside the deciding councils, whose composition is dominated by adult male kin heads with standing to fight or negotiate.
% DISAPPEARANCE_RATIONALE: If the feud obligation vanished overnight with no substitute mechanism, the deterrent function it provides would disappear along with it — casual violence would face no credible collective response, and aggrieved parties would have no recognized channel for redress until or unless an alternative (courts, police, a new compensation regime) emerged to fill the gap.
% FOUNDING_PROBLEM: In stateless or weakly-stated societies, there was no institution capable of investigating a killing, establishing guilt, or compelling a wrongdoer to pay or suffer a penalty — without SOME mechanism, killings would go entirely unanswered and violence would be unconstrained.
% FOUNDING_PROBLEM_CORROBORATION: Comparative legal historians studying Icelandic sagas, Albanian kanun records, and early Germanic law codes attest that in documented cases wergild schedules functioned as real, negotiated substitutes for violence and that feud-capable regions show measurably lower rates of casual homicide relative to areas with neither feud nor state enforcement — corroboration from outside the kin groups that benefit from the arrangement. Ecclesiastical chroniclers of the same period, however, describe multi-generational feud cycles that persisted well past any plausible original grievance, suggesting the founding problem's status is genuinely disputed rather than settled in the coordination reading's favor.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.32, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-moderate (0.32) because, under this reading, the primary flow is mutual: kin commit reciprocal liability in exchange for reciprocal protection, and compensation (wergild) is a real, frequently-chosen substitute for violence rather than a captured toll. Suppression is authored low (0.28) specifically because this reading holds that wergild and other settlement paths coexist and are not foreclosed by the feud norm — a defining structural feature of the coordination reading as distinct from its siblings. Resistance is moderate (0.35): defection does occur and carries real cost, which is the honest cost side of an otherwise low-suppression coordination story.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin group members and aggrieved lineages are declared beneficiaries because, under this reading, the obligation subsidizes their security and provides recourse they would otherwise lack — this pulls their derived directionality toward the beneficiary end. Feud defectors and outcast kin are declared victims because refusing the obligation triggers honor loss and expulsion — a real cost imposed specifically on those who decline to participate, which is structurally different from the sibling readings' victim sets (which would center on cyclical-violence casualties or defiers of ecclesiastical authority). This reading's victims are defectors from coordination, not targets of extraction by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/founding_problem_status pairing is authored as contested rather than resolved in either direction: comparative legal-historical evidence (from outside the benefiting kin groups) supports genuine deterrent function in some documented cases, while ecclesiastical chronicle evidence documents multi-generational cycles outliving any plausible triggering grievance. This story does not resolve that tension — it is precisely the tension the extraction_cycle_reading exists to press from the opposite direction. Declaring the tension honestly here, rather than resolving it in this reading's favor, is what keeps the reading's ε from being tuned to a predetermined rope verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_kernel_reading_selection,
    'Is the feud obligation kernel best read as genuine stateless coordination (this reading), a destructive extraction cycle (extraction_cycle_reading), or an illegitimate usurpation of properly centralized/divine violence authority (christianized_pacification_reading)?',
    'Comparative case analysis across documented feud societies: measure homicide rates, cycle duration, and wergild settlement frequency against regions with centralized courts and against regions with neither mechanism; separately assess ecclesiastical/royal legitimacy claims on their own doctrinal terms.',
    'If extraction-cycle evidence dominates (long unresolved cycles, net productive-capacity loss), this reading''s low-extraction, low-suppression metrics would be revealed as reading-selection rather than fact; if coordination evidence dominates, the extraction_cycle_reading''s high-ε claim would be the outlier instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_kernel_reading_selection, conceptual, 'Which of three contested readings of the same feud-obligation kernel best fits the historical record.').

omega_variable(
    wergild_genuineness_of_alternative,
    'Was wergild a genuinely accessible, non-suppressed alternative to violent retaliation, or was it available in practice only to lineages with enough standing/wealth to negotiate, effectively suppressing the alternative for the powerless?',
    'Examine wergild schedules and settlement records for evidence of differential access by kin-group wealth or status; compare rates of actual settlement versus violent retaliation across social strata.',
    'If wergild access was itself stratified, the low-suppression claim central to this reading weakens, and defectors/outcast kin would need to be reclassified with higher effective suppression — moving this reading structurally closer to the tangled_rope territory the sibling extraction reading already occupies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_genuineness_of_alternative, empirical, 'Whether the wergild alternative was genuinely open or itself gated by status.').

omega_variable(
    defector_exit_severity,
    'How severe and how reversible was expulsion for feud defectors — a temporary honor cost recoverable through later compliance, or a permanent, life-threatening exile?',
    'Legal-anthropological case studies of documented outcast status (e.g., Icelandic outlawry, skoggangr) tracing recovery rates and post-expulsion mortality/survival outcomes.',
    'If expulsion was near-permanent and life-threatening, feud_defectors and outcast_kin should be weighted as bearing severe rather than moderate cost, raising this reading''s effective extraction on those seats even while the coordination benefit for compliant kin remains real.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(defector_exit_severity, empirical, 'Severity and reversibility of the cost borne by feud-obligation defectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 60, 0.11).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 80, 0.12).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.3).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 60, 0.32).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 80, 0.32).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.32).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feud_obligation_kernel__stateless_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraints reading the same feud_obligation_kernel. stateless_coordination_reading (this file) treats the obligation as genuine self-enforcing coordination with low suppression (wergild coexists) and locates victimhood in defection, not participation. extraction_cycle_reading treats the same kernel as a destructive rent cycle with high extraction and victims among casualties of prolonged, self-perpetuating violence. christianized_pacification_reading treats the kernel as a usurpation of legitimate (divine/institutional) violence authority, with its own distinct beneficiary/victim structure centered on ecclesiastical and royal claimants. All three share the kernel_id but are authored as separate files with independent ε values per the ε-invariance principle; they are linked here via affects_constraints rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
