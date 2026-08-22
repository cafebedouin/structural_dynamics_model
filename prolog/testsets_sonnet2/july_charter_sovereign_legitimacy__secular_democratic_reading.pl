% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter — Secular Democratic Reading of Sovereign Legitimacy
 *   domain: constitutional_law/political_transitions/post_revolutionary_state_building
 *
 * SUMMARY:
 *   Following the collapse of the prior regime, a July Charter was adopted to
 *   provide a foundational text for the transition's constitutional order.
 *   The Charter's actual language on sovereignty is contested by at least
 *   three organized readings, each of which instantiates a structurally
 *   distinct constraint with a different beneficiary/victim set: a
 *   guided-nationalism reading grounding legitimacy in Islamic-national
 *   identity, a military-custodian reading ratifying the armed forces as a
 *   permanent stability guarantor, and this secular-democratic reading, which
 *   mandates civilian democratic institutions with the military subordinate
 *   to elected civilian authority and religious-political claims
 *   constitutionally constrained. This story authors ONLY the
 *   secular-democratic reading as its own ε-invariant constraint. The sibling
 *   readings are separate constraints, not alternative measurements of this
 *   one, and their ε values are not reconciled here.
 *
 * KEY AGENTS:
 *   - secular_democratic_coalition_parties: agenda_setter (organized/constrained) — administers the interpretive apparatus and drafts implementing legislation
 *   - jamaat_e_islami: primary target (organized/constrained) — religious-nationalist legitimacy claim structurally excluded
 *   - military_autonomous_authority: primary target (institutional/constrained) — autonomous guardianship claim subordinated to civilian oversight
 *   - constitutional_tribunal_arbiters: analytical observer (institutional/analytical) — adjudicates which reading governs contested cases
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.62).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter — Secular Democratic Reading of Sovereign Legitimacy").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions/post_revolutionary_state_building").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '0d6b539f-fd92-49be-88b8-9baeb4bae2df').
narrative_ontology:cs_kernel_codification('0d6b539f-fd92-49be-88b8-9baeb4bae2df', formalized).
narrative_ontology:cs_authority_grounding('0d6b539f-fd92-49be-88b8-9baeb4bae2df', lineage).
narrative_ontology:cs_interpretation_layer_present('0d6b539f-fd92-49be-88b8-9baeb4bae2df').
narrative_ontology:cs_reading_relation('0d6b539f-fd92-49be-88b8-9baeb4bae2df', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('0d6b539f-fd92-49be-88b8-9baeb4bae2df', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('0d6b539f-fd92-49be-88b8-9baeb4bae2df', foundational, popular_sovereignty_grounds_legitimacy).
narrative_ontology:cs_axiom_status(popular_sovereignty_grounds_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('0d6b539f-fd92-49be-88b8-9baeb4bae2df', popular_sovereignty_grounds_legitimacy, deontological).
narrative_ontology:cs_axiom('0d6b539f-fd92-49be-88b8-9baeb4bae2df', foundational, military_authority_subordinate_to_elected_civilians).
narrative_ontology:cs_axiom_status(military_authority_subordinate_to_elected_civilians, holdable).
narrative_ontology:cs_axiom_grounding('0d6b539f-fd92-49be-88b8-9baeb4bae2df', military_authority_subordinate_to_elected_civilians, conventional).
narrative_ontology:cs_reference_frame('0d6b539f-fd92-49be-88b8-9baeb4bae2df', civilian_electoral_sovereignty_framework).
narrative_ontology:cs_drift_state('0d6b539f-fd92-49be-88b8-9baeb4bae2df', post_charter_second_year, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0d6b539f-fd92-49be-88b8-9baeb4bae2df', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_coalition_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_reform_networks).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, judicial_reform_advocates).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, international_donor_governments).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted and champion the Charter's secular-democratic reading, positioning constitutional legitimacy on popular sovereignty and civilian institutional supremacy rather than religious or military authority. They administer the interpretive apparatus that determines which actors count as legitimate participants in the transition, and they benefit from a settlement that structurally advantages parties committed to secular electoral competition over religiously-organized or military-backed rivals.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_coalition_parties, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_coalition_parties, beneficiary).

% Lawyers, journalists, and civic organizers who mobilized for the transition and gain protected space under a secular-democratic settlement — freedom of association, press protections, judicial independence — that would be narrower under either a religiously-grounded or military-guardianship reading.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civil_society_reform_networks, beneficiary,
    moderate, generational, constrained, national).

% Constitutional lawyers and reform-minded judges who gain institutional authority to review both religious-political claims and military prerogatives under a civilian-supremacy framework; their professional standing and doctrinal project depend on the secular reading prevailing.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, judicial_reform_advocates, beneficiary,
    moderate, generational, constrained, national).

% An organized religious-political movement with a substantial mobilization base whose claim to shape sovereign legitimacy on Islamic-nationalist grounds is structurally excluded or constrained by the secular reading. They can contest through electoral politics and street mobilization but the Charter's secular-democratic interpretation forecloses their preferred constitutional premise from being the settlement's ground truth; exit from the political system is not realistic, but exit from marginalization requires either reinterpreting the Charter or building enough electoral force to force a renegotiation.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, constrained, national).

% The armed forces' institutional claim to stand as an autonomous guarantor of order — able to intervene, remove governments, or set boundaries on civilian policy without electoral accountability — is subordinated under this reading to elected civilian oversight. The military retains coercive capacity and could resist implementation, but formal constitutional subordination strips the legal cover for autonomous intervention that the custodian reading would preserve.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% Voters who understand national identity and legitimate governance in religious-nationalist terms find their preferred vision of the state's foundational identity constitutionally deprioritized. They retain the vote but the terms of legitimate political contest have been redrawn around secular-democratic premises they did not choose and cannot easily exit, since citizenship itself is the only available frame.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_conservative_electorate, payer,
    powerless, biographical, trapped, national).

% Foreign governments and multilateral institutions that prefer a secular-democratic settlement as a condition for aid, trade, and diplomatic normalization; they benefit from a reading that aligns the country's institutions with frameworks they can engage without dealing with either a religious-nationalist state or an openly military-ruled one, and they can withdraw support if the reading fails to consolidate without bearing domestic costs themselves.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_donor_governments, beneficiary,
    institutional, biographical, arbitrage, global).

% Judges and constitutional scholars tasked with interpreting which reading of the Charter's sovereignty clause governs contested cases; they hear claims from all three readings' proponents and their rulings will determine, in practice, how much of the secular-democratic premise actually becomes enforceable law versus remaining aspirational text.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, constitutional_tribunal_arbiters, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_coalition_parties).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared constitutional ground rule — popular sovereignty exercised through elected civilian institutions — that lets rival post-revolutionary factions compete for power through a common electoral and judicial framework instead of through armed contest or religious authority claims, reducing the risk of renewed civil conflict over what kind of state exists at all.
% TRANSFER_FUNCTION: Moves interpretive and institutional authority away from religious-political movements and the military's autonomous guardianship claim and toward elected civilian bodies and the judiciary that enforces civilian supremacy; correspondingly moves legitimacy-conferring status toward secular-democratic parties and away from Jamaat-e-Islami's religious-nationalist claim and the military's custodian claim.
% ABSENT_VOICES: Jamaat-e-Islami's constitutional theorists and religious-conservative constitutional scholars are marginal or absent from the drafting and interpretive bodies that fix what the Charter's sovereignty clause means; military legal counsel participates but under a reading that treats their institutional preferences as subordinate inputs, not co-equal authorship. Both would object that the secular reading was adopted by drafters already committed to excluding them rather than negotiated as a genuine settlement among all post-revolutionary factions.
% DISAPPEARANCE_RATIONALE: If the secular-democratic reading were abandoned as the operative interpretation, the constitutional space now claimed by civilian democratic institutions would very likely be filled either by a religious-nationalist reconstitution of sovereignty (advantaging Jamaat-e-Islami and allied movements) or by formalized military guardianship — both alternatives already exist as competing readings of the same kernel text and are actively argued for by organized constituencies waiting for the secular reading to fail or be reinterpreted.
% FOUNDING_PROBLEM: The prior regime's collapse left a legitimacy vacuum: no settled answer existed for whether the state's authority would rest on popular sovereignty, religious-national identity, or military guardianship, and the transition needed a foundational text that could stop the contest from being settled by force.
% FOUNDING_PROBLEM_CORROBORATION: Secular-democratic coalition members and international donor governments attest the founding problem — establishing civilian democratic legitimacy against both theocratic and military alternatives — remains live and unresolved. Independent constitutional scholars outside the coalition, and Jamaat-e-Islami's own legal representatives, corroborate that a foundational legitimacy question was genuinely open at the point of drafting, but dispute that the secular reading represents a neutral resolution rather than one faction's victory encoded as constitutional settlement; no source outside the secular coalition itself attests that the secular reading was the only defensible resolution of the founding problem.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored moderate-high (0.58 by interval end) because the secular-democratic reading does not merely coordinate a shared framework — it actively reallocates legitimacy-conferring authority away from two organized, resourced rivals (Jamaat-e-Islami's mobilization base, the military's coercive institutional weight) toward the coalition administering the interpretation and the judiciary it constitutes. Suppression (0.62) reflects that this reading's persistence depends on active enforcement: legal exclusion mechanisms, judicial rulings foreclosing religious-nationalist constitutional claims, and formal subordination of military prerogatives that the military has historically resisted. Theater ratio (0.40) is elevated but not dominant — genuine institution-building (electoral bodies, judicial review structures) is occurring, but a meaningful share of activity is performative signaling toward international donors that civilian supremacy is settled when implementation against military resistance remains incomplete. The rising trajectories across all three tracked metrics on one shared grid reflect a reading whose enforcement apparatus hardened as rival factions tested its limits over the two years following adoption.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular-democratic coalition and allied civil society/judicial networks sit near the full-beneficiary end: they authored the interpretive project, occupy the institutions it creates, and gain protected space they would lack under either sibling reading. Jamaat-e-Islami and the military's autonomous-authority claim sit near the full-target end: both are organized, powerful actors whose constitutional entitlement claims are constrained or subordinated by this specific reading, and neither has a low-cost exit — Jamaat-e-Islami cannot exit national politics, and the military cannot exit the country whose constitution now formally subordinates it. The religious-conservative electorate is powerless and trapped, bearing the reading's redefinition of legitimate political identity without the organizational leverage its allied movement possesses. International donors are beneficiaries with arbitrage-grade exit — they can withdraw support without domestic cost, which is precisely why their preference for this reading carries real weight in sustaining it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a stable legitimacy ground amid genuine multi-factional contest — was real and remains partly live: no faction has yet achieved uncontested victory, so the arrangement cannot be dismissed as pure inertia. But the reading's own coalition treats the founding problem as already resolved in its favor, which risks converting an ongoing three-way constitutional contest into a fait accompli enforced through judicial and legal mechanisms rather than through the broader multi-factional negotiation the transition initially required. Classifying this as tangled_rope rather than either pure rope (which would understate the exclusion of organized rivals) or pure snare (which would understate the genuine coordination value of preventing the vacuum from resolving through violence) prevents both false-summit legitimation and an over-corrected dismissal of the settlement's real stabilizing function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_text_underdetermination,
    'Does the Charter''s actual sovereignty-clause language determinately support the secular-democratic reading, or is the text itself ambiguous enough that all three readings are equally textually defensible, making the eventual winner a function of who controls implementation rather than what was written?',
    'Close textual and drafting-history analysis of the Charter''s sovereignty clause, including drafting committee records and comparison with contemporaneous statements by drafters across factions, to establish whether the text itself privileges one reading or was left deliberately ambiguous as a compromise.',
    'If the text is genuinely underdetermined, this reading''s claim to represent the Charter''s actual mandate is weaker than the coalition''s own framing suggests, and the tangled_rope classification''s extraction component is better understood as the coalition''s interpretive victory rather than textual fidelity — strengthening the case that this is one faction''s constitutional project rather than a neutral coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_text_underdetermination, conceptual, 'Whether the Charter text determinately supports this reading or is contested-ambiguous.').

omega_variable(
    military_compliance_durability,
    'Will formal constitutional subordination actually bind the military''s behavior over time, or does the military retain sufficient coercive capacity and political leverage to functionally resume autonomous guardianship regardless of what this reading''s implementing institutions declare?',
    'Track military behavior at the next major political crisis point: does the military defer to civilian institutions under contested conditions, or does it act unilaterally, revealing the constitutional subordination as more aspirational than operative?',
    'If the military''s autonomous authority persists in practice, the secular-democratic reading''s classification should shift toward scaffold (a transitional arrangement not yet load-bearing) or toward higher theater_ratio (subordination as performance) rather than a stably enforced tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_compliance_durability, empirical, 'Whether military subordination under this reading is durable or aspirational.').

omega_variable(
    jamaat_exclusion_proportionality,
    'Is the constitutional constraint on Jamaat-e-Islami''s political participation proportionate to a genuine coordination need (preventing religious-authoritarian capture of the transition) or does it constitute disproportionate exclusion of a legitimately mobilized political constituency from constitutional authorship?',
    'Comparative analysis of how the Charter treats Jamaat-e-Islami''s electoral participation versus its constitutional-interpretive standing — full electoral participation with constitutional-authorship exclusion looks different from wholesale political exclusion, and the two support different extraction assessments.',
    'If exclusion is limited to constitutional-authorship standing while electoral participation remains open, extraction is lower than currently authored; if exclusion extends into electoral or associational restriction, extraction and suppression are understated here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jamaat_exclusion_proportionality, conceptual, 'Whether Jamaat-e-Islami''s exclusion from constitutional authorship is proportionate or extends into broader political exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(july_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(july_tr_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(july_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(july_tr_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(july_tr_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(july_tr_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(july_tr_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(july_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(july_be_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 4, 0.44).
narrative_ontology:measurement(july_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(july_be_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(july_be_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(july_be_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(july_be_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 24, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(july_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(july_su_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 4, 0.5).
narrative_ontology:measurement(july_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(july_su_t12, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 12, 0.58).
narrative_ontology:measurement(july_su_t16, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(july_su_t20, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement(july_su_t24, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 24, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.12).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% This story is one of three constraint files decomposing the natural-language concept 'the July Charter's sovereignty mandate' per the ε-invariance principle. Each reading (secular_democratic, guided_nationalism, military_custodian) is authored as a separate constraint with its own ε, beneficiary/victim structure, and claimed type, because evaluating the same Charter text against different sovereignty-grounding premises yields materially different extraction profiles and inverted beneficiary/victim sets. The three stories are linked via affects_constraints rather than merged, following the BGS decomposition pattern: they share a kernel (the Charter text and the founding legitimacy crisis) but instantiate structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
