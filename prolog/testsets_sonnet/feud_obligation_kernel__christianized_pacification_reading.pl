% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Ecclesiastical Prohibition of Blood-Feud as Divine Law Violation
 *   domain: legal anthropology / medieval history / comparative political systems
 *
 * SUMMARY:
 *   Beginning in the early medieval period, ecclesiastical authorities
 *   articulated blood-feud vengeance as a violation of divine law — private
 *   retaliation usurps an authority God reserves to Himself and delegates
 *   only to anointed institutions (Church and Crown). Peace of God and Truce
 *   of God movements, penitential manuals prescribing pilgrimage or
 *   excommunication for feud violence, and royal legislation criminalizing
 *   self-help killing together constituted a sustained campaign to relocate
 *   legitimate violence-authority into centralized hands. This story
 *   instantiates ONLY the christianized_pacification_reading of the
 *   feud_obligation_kernel: the claim that feud is a theological and
 *   jurisdictional violation requiring suppression. It does not adjudicate
 *   whether feud was 'really' a functional coordination mechanism
 *   (stateless_coordination_reading) or a destructive extraction cycle
 *   undermining consolidation (extraction_cycle_reading) — those are separate
 *   constraints with separate ε values, linked here via
 *   network.affects_constraints. Within THIS reading, extraction rises over
 *   the interval as the Church's and Crown's jurisdictional apparatus matures
 *   from occasional council pronouncement into standing penitential and
 *   judicial infrastructure with real fee, land, and fine flows.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.61).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.72).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.66).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Ecclesiastical Prohibition of Blood-Feud as Divine Law Violation").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal anthropology / medieval history / comparative political systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, 'e052bc0d-983e-4ef7-a2eb-21c21b752681').
narrative_ontology:cs_kernel_codification('e052bc0d-983e-4ef7-a2eb-21c21b752681', formalized).
narrative_ontology:cs_authority_grounding('e052bc0d-983e-4ef7-a2eb-21c21b752681', lineage).
narrative_ontology:cs_interpretation_layer_present('e052bc0d-983e-4ef7-a2eb-21c21b752681').
narrative_ontology:cs_reading_relation('e052bc0d-983e-4ef7-a2eb-21c21b752681', feud_obligation_kernel__stateless_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('e052bc0d-983e-4ef7-a2eb-21c21b752681', feud_obligation_kernel__extraction_cycle_reading, influences).
narrative_ontology:cs_axiom('e052bc0d-983e-4ef7-a2eb-21c21b752681', foundational, vengeance_authority_reserved_to_god).
narrative_ontology:cs_axiom_status(vengeance_authority_reserved_to_god, holdable).
narrative_ontology:cs_axiom_grounding('e052bc0d-983e-4ef7-a2eb-21c21b752681', vengeance_authority_reserved_to_god, theological).
narrative_ontology:cs_axiom('e052bc0d-983e-4ef7-a2eb-21c21b752681', foundational, violence_authority_delegable_only_through_ordained_office).
narrative_ontology:cs_axiom_status(violence_authority_delegable_only_through_ordained_office, holdable).
narrative_ontology:cs_axiom_grounding('e052bc0d-983e-4ef7-a2eb-21c21b752681', violence_authority_delegable_only_through_ordained_office, conventional).
narrative_ontology:cs_reference_frame('e052bc0d-983e-4ef7-a2eb-21c21b752681', apostolic_prohibition_of_private_vengeance).
narrative_ontology:cs_drift_state('e052bc0d-983e-4ef7-a2eb-21c21b752681', high_medieval_peace_movement_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('e052bc0d-983e-4ef7-a2eb-21c21b752681', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, episcopal_and_monastic_church).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_courts_claiming_delegated_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, avenging_kinsmen).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, targeted_lineages).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, peace_breakers_subject_to_penance).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, local_lords_and_peace_guarantors).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_lords_and_peace_guarantors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declares vengeance killing a mortal sin (violation of Romans 12:19 and canon prohibitions on private violence), builds a penitential apparatus of confession, excommunication, sanctuary law, and Peace/Truce of God movements to route disputes toward compurgation, wergild, or ecclesiastical arbitration instead of retaliation. Collects tithes, land grants, and jurisdictional reach wherever it substitutes its own courts and penances for kin-based settlement. Its authority to declare feud sinful is itself the mechanism by which it expands into secular dispute resolution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, episcopal_and_monastic_church, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, episcopal_and_monastic_church, beneficiary).

% Kings and their courts assert that legitimate coercive force flows from God through anointed royal office, criminalizing self-help violence that bypasses the crown's judicial apparatus. Collects fines, forfeitures, and expanded taxing/judicial reach each time a feud is redirected into a royal court. Benefits directly from the same theological premise the Church supplies, even where crown and Church later contest which institution holds the delegation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_courts_claiming_delegated_authority, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, royal_courts_claiming_delegated_authority, beneficiary).

% Bound by kin-group honor obligation to avenge a slain relative, but now told that carrying out the obligation places their soul in mortal peril, subjects them to excommunication, and may bring royal prosecution alongside continued blood-price liability. Caught between two competing authority claims — kin honor and Christian/royal law — with no clean exit: refusing vengeance risks social death within the kin group, performing it risks spiritual and legal death under the new order.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, avenging_kinsmen, payer,
    moderate, biographical, constrained, local).

% The kin group on the receiving end of a feud claim now finds ecclesiastical and royal machinery inserted into what was previously a negotiated settlement process; they must navigate sanctuary claims, compurgation oaths, and wergild payments administered by an institution taking a cut and asserting jurisdiction, on top of whatever settlement the original feud logic would have produced.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, targeted_lineages, payer,
    moderate, biographical, constrained, local).

% Individuals who act on feud obligation after the prohibition is promulgated are treated as sinners requiring public penance, pilgrimage, or excommunication, and as criminals subject to royal fine or execution — a doubled liability layered on top of the pre-existing blood-price exposure they already carried under the older kin-settlement logic.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, peace_breakers_subject_to_penance, payer,
    powerless, biographical, trapped, local).

% Regional magnates who host Peace of God councils or enforce truce periods gain standing as guarantors of divinely sanctioned order and can extract fees or loyalty for the service, but are also bound by the same prohibition when their own retainers are implicated in feud violence, and bear enforcement costs when truces are broken on their territory.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_lords_and_peace_guarantors, beneficiary,
    powerful, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__christianized_pacification_reading, local_lords_and_peace_guarantors, payer).

% Ordinary parishioners living amid feuding kin groups bear the collateral costs of both the original violence and the new penitential and judicial apparatus layered atop it, but have no voice in whether the Church's or Crown's competing jurisdictional claims are legitimate; their preference for whichever mechanism actually stops the killing is not consulted.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, village_and_parish_communities, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__christianized_pacification_reading, episcopal_and_monastic_church).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__christianized_pacification_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The prohibition redirects private violence into centralized channels (confession, penance, ecclesiastical arbitration, royal courts), which does reduce cycles of retaliatory killing when the substitute institutions function — a real coordination gain over unmediated feud escalation, when it works.
% TRANSFER_FUNCTION: Moves jurisdictional authority, dispute-resolution fees, land grants, and moral standing from kin groups and their customary settlement mechanisms (wergild negotiated peer-to-peer) to the Church's penitential system and the Crown's judicial apparatus, while imposing a doubled liability (spiritual plus secular) on anyone who continues to act under the older feud logic.
% ABSENT_VOICES: Kin-group elders and customary law-speakers who administered the older wergild and compensation system are structurally excluded from the theological argument — the prohibition is issued as divine law, not negotiated with the parties whose dispute-resolution authority it displaces. Village communities living with the practical consequences of contested jurisdiction have no seat at either the ecclesiastical council or the royal court.
% DISAPPEARANCE_RATIONALE: The Church and Crown would say the world rearranges catastrophically — vengeance killings resume unchecked, souls are lost, order collapses. Kin groups accustomed to functioning wergild systems (see the sibling stateless_coordination_reading) would say their own mechanism was already handling deterrence and settlement, and its supersession by ecclesiastical/royal jurisdiction was itself the disruption, not the fix. Whether removing THIS prohibition specifically (as opposed to removing feud practice itself) causes the world to rearrange depends entirely on which underlying claim about feud's function you credit — hence contested rather than settled either way.
% FOUNDING_PROBLEM: Endemic retaliatory killing among kin groups, understood within this reading as a violation of divine law reserving vengeance to God (Romans 12:19, Deuteronomy 32:35) and as a usurpation of the legitimate violence-authority properly delegated to anointed rulers and the Church's penitential discipline.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical chronicles and conciliar records (Peace of God, Truce of God councils, penitential handbooks) attest the problem as live and the Church's intervention as necessary and ongoing. Independent corroboration is thin: comparative legal-anthropological scholarship on stateless dispute-resolution systems (the basis for the stateless_coordination_reading) argues the underlying disorder was already partially self-regulating via wergild before ecclesiastical intervention, suggesting the 'divine law violation' framing was itself a jurisdictional claim advanced by an interested party rather than a neutral description of a prior vacuum. No source fully outside the Church's or Crown's own institutional interest attests that feud was, in fact, theologically illegitimate rather than merely inconvenient to centralizing authorities.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, contested).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts moderate (0.32) reflecting early, largely rhetorical conciliar pronouncements, and rises to 0.61 as the apparatus of penance, excommunication, sanctuary law, and royal fine collection matures into a durable revenue and jurisdiction-generating machine layered on top of (not simply replacing) the costs feuding parties already bore. Suppression is high throughout and rises further (0.45→0.72) as enforcement mechanisms — excommunication, interdiction, royal prosecution — harden from persuasion into compulsion. Theater ratio is moderate and rising (0.18→0.44): substantial genuine coordination function persists (dispute resolution does reduce some killing), but an increasing share of the apparatus — elaborate penitential tariffs, jurisdictional disputes between Church and Crown courts over who properly 'delegates' violence-authority — serves institutional position-taking rather than actually reducing violence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Church and royal courts are declared beneficiaries: both gain expanded jurisdiction, revenue, and moral/legal standing precisely by asserting that feud violates divine law, positioning themselves as the sole legitimate channel. Every feud participant — avenger, target lineage, and any peace-breaker who acts on the old obligation — enters the victim set under this reading, per the expected structural delta: the prohibition does not simply redirect their dispute, it places their souls in jeopardy and doubles their legal exposure (spiritual penance plus secular fine/execution) without necessarily providing a settlement mechanism as fast or as locally legitimate as the kin-based wergild system it displaces. Local lords occupy a hybrid position — beneficiaries when they host peace councils and collect associated standing, payers when their own retainers are caught in the prohibition's net.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem interview deliberately routes through corroboration rather than self-assertion: ecclesiastical sources say the problem (unauthorized private violence) remains live and their intervention necessary; but the only source that could corroborate the underlying theological premise from OUTSIDE the beneficiary set — comparative legal anthropology of the settlement mechanisms feud actually replaced — instead suggests the 'divine law violation' framing was a jurisdictional claim, not a neutral diagnosis. This does not resolve the omega; it is exactly the kind of founding-problem/beneficiary-corroboration mismatch the six-questions battery exists to surface, distinguishing this from a case where a genuine coordination failure was later captured (mandatrophy) versus a case where the coordination frame was authored by the capturing party from the outset.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_premise_vs_jurisdictional_interest,
    'Is the claim that private vengeance violates divine law a genuine theological discovery independent of institutional interest, or is it a doctrine whose content was shaped by the jurisdictional and revenue interests of the institutions that promulgated it?',
    'Comparative analysis of the doctrine''s development timeline against the Church''s and Crown''s jurisdictional expansion timeline; examination of whether penitential tariffs and royal fines scaled with actual violence reduction or with institutional capacity to enforce.',
    'If the doctrine tracks institutional interest more closely than violence outcomes, the coordination function claimed by this reading is substantially cover for extraction, pushing the computed type toward snare; if the doctrine precedes and is independent of the institutional apparatus built on it, the coordination claim has more independent standing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_premise_vs_jurisdictional_interest, conceptual, 'Whether the divine-law-violation framing is theologically prior or institutionally constructed.').

omega_variable(
    committer_kernel_reading_selection,
    'The feud_obligation_kernel supports at least three structurally distinct readings (this pacification reading, the stateless_coordination_reading, and the extraction_cycle_reading) with materially different ε values and victim/beneficiary sets — which reading a given historical source or modern historian selects is itself a contested act. What determines which reading a given medieval or modern observer adopts?',
    'Track correlation between an observer''s institutional position (churchman, royal official, kin-group descendant, modern legal anthropologist) and which reading they report as descriptively true; a strong correlation would indicate reading-selection is itself interest-driven rather than evidence-driven.',
    'If reading-selection correlates strongly with institutional position, none of the three readings can claim to be the neutral baseline description of feud — each is a committer-relative account, and the kernel itself (feud''s normative status) remains genuinely underdetermined by the evidence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_kernel_reading_selection, conceptual, 'Whether kernel-reading selection is evidence-driven or interest-driven across the three sibling readings.').

omega_variable(
    penitential_suppression_completeness,
    'Did penitential discipline (confession, excommunication, pilgrimage) actually achieve the complete suppression of feud violence it sought, or did feud practice persist informally alongside nominal compliance with the prohibition?',
    'Court records and chronicle evidence of continued feud violence after formal prohibition, cross-referenced with penitential register entries, to assess actual compliance versus nominal/theatrical compliance.',
    'High persistence of informal feud practice despite the prohibition would support a higher theater_ratio and suggest the suppression apparatus functioned more as jurisdictional/revenue theater than as effective violence reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(penitential_suppression_completeness, empirical, 'Whether penitential suppression achieved real behavioral change or primarily nominal compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.25).
narrative_ontology:measurement(feud_tr_t120, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 120, 0.31).
narrative_ontology:measurement(feud_tr_t200, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 200, 0.37).
narrative_ontology:measurement(feud_tr_t300, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 300, 0.41).
narrative_ontology:measurement(feud_tr_t400, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 400, 0.44).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.41).
narrative_ontology:measurement(feud_be_t120, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 120, 0.49).
narrative_ontology:measurement(feud_be_t200, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 200, 0.55).
narrative_ontology:measurement(feud_be_t300, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 300, 0.59).
narrative_ontology:measurement(feud_be_t400, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 400, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.55).
narrative_ontology:measurement(feud_su_t120, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 120, 0.62).
narrative_ontology:measurement(feud_su_t200, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 200, 0.68).
narrative_ontology:measurement(feud_su_t300, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 300, 0.71).
narrative_ontology:measurement(feud_su_t400, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 400, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__christianized_pacification_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, stateless_coordination_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__christianized_pacification_reading, extraction_cycle_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of feud_obligation_kernel. All three share the same underlying historical phenomenon (kin-group blood-feud obligation in stateless or weakly-centralized medieval polities) but assign different ε values, different beneficiary/victim sets, and different claimed types depending on which normative frame is adopted: divine-law violation requiring ecclesiastical/royal suppression (this story, tangled_rope), self-enforcing coordination mechanism (stateless_coordination_reading, likely rope or tangled_rope with much lower suppression), or destructive extraction cycle blocking consolidation (extraction_cycle_reading, likely snare with different victims). Per the ε-invariance principle, these are three constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
