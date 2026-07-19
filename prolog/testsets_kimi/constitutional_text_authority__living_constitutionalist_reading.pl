% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__living_constitutionalist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Living Constitutionalist Reading of Constitutional Text Authority
 *   domain: legal/constitutional/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The U.S. Constitution is a fixed, very short, very hard-to-amend text,
 *   and three rival accounts of what makes it authoritative form a constraint
 *   family. This story instantiates ONE reading — the living
 *   constitutionalist reading: constitutional meaning legitimately evolves
 *   with social attitudes and values, and judicial application of the text's
 *   broad principles to changed circumstances is fidelity, not betrayal.
 *   Under this reading the federal judiciary becomes the standing amendment
 *   mechanism; Brown v. Board (1954) is the canonical warrant — school
 *   segregation moved from constitutionally permissible to unconstitutional
 *   with no Article V event. The reading solves a real coordination problem
 *   (a dead-hand text governing a changed society) and simultaneously
 *   transfers change-authority from amendment coalitions and legislative
 *   majorities to courts and the professional culture that staffs them. Its
 *   enforcement is professional as much as juridical: for most of the
 *   interval rival methodologies were marginalized in elite legal
 *   institutions, and the values-gate was operationalized by that same
 *   professional culture. Per ε-invariance, the sibling readings
 *   (originalist, positivist) are separate constraints with different
 *   beneficiary/victim structures and different ε; this file models only the
 *   living reading. The claimed_type (tangled_rope) is my structural judgment
 *   from the authoring seat — genuine coordination function plus asymmetric
 *   authority transfer through the same apparatus — stated independently of
 *   the authored metrics, which describe the regime's actual operation.
 *
 * KEY AGENTS:
 *   - federal_judiciary: Primary agenda_setter and gain recipient (institutional/identity_locked) — administers the interpretive regime; constitutional change-authority accrues to it
 *   - legal_academic_establishment: Primary beneficiary (organized/identity_locked) — paradigm dominance supplies careers, prestige, and staffing power
 *   - rights_claiming_litigants: Secondary beneficiary (moderate/constrained) — gain a recognition channel unavailable through Article V, pay litigation costs and uncertainty
 *   - displaced_democratic_majorities: Primary payer (organized/constrained) — enact policies invalidated under elaborated standards; amendment remedy practically sealed
 *   - rival_interpretive_schools: Payer (organized/constrained) — excluded from elite institutions for decades; organized an insurgency that eventually captured judicial seats
 *   - state_governments: Payer (institutional/constrained) — policy space narrowed by expanding federal constitutional floors
 *   - popular_constitutionalist_publics: Excluded (powerless/trapped) — their constitutional meanings are raw material for the values-gate, not law themselves
 *   - comparative_constitutional_scholars: Analytical observer — supply external evidence on amendment difficulty and judicial-power variance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.48).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.5).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Living Constitutionalist Reading of Constitutional Text Authority").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "legal/constitutional/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'b1b7e737-1bc7-443e-89f6-b8cf5769f72b').
narrative_ontology:cs_kernel_codification('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', fixed_text).
narrative_ontology:cs_authority_grounding('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', lineage).
narrative_ontology:cs_interpretation_layer_present('b1b7e737-1bc7-443e-89f6-b8cf5769f72b').
narrative_ontology:cs_reading_relation('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', constitutional_text_authority__originalist_reading, forecloses).
narrative_ontology:cs_reading_relation('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', constitutional_text_authority__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', foundational, constitutional_meaning_evolves_with_contemporary_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_contemporary_values, holdable).
narrative_ontology:cs_axiom_grounding('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', constitutional_meaning_evolves_with_contemporary_values, deontological).
narrative_ontology:cs_axiom('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', secondary, judicial_adaptation_constitutes_fidelity).
narrative_ontology:cs_axiom_status(judicial_adaptation_constitutes_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', judicial_adaptation_constitutes_fidelity, instrumental).
narrative_ontology:cs_reference_frame('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', living_charter_broad_principles).
narrative_ontology:cs_drift_state('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', contemporary_originalist_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b1b7e737-1bc7-443e-89f6-b8cf5769f72b', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, legal_academic_establishment).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, rights_claiming_litigants).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, displaced_democratic_majorities).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, rival_interpretive_schools).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_governments).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, unenumerated_rights_recognizability).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, evolving_standards_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text_authority__living_constitutionalist_reading, brown_v_board_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the constitutional text through doctrinal elaboration in the cases before it, applying broad provisions to present circumstances. Receives the disputes that define national rights and policy boundaries; its precedents accumulate into the operative constitution. Its standing depends on being the forum where constitutional meaning is settled; accepting a reduced interpretive role would be a self-diminishment the institution has never voluntarily performed.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Produces the theory, doctrine, and trained lawyers that staff the interpretive institutions. For most of the interval its dominant paradigm treated evolving constitutional meaning as settled methodology; careers, tenure, casebook franchises, and prestige were built on doctrinal elaboration. Rival methodologies were, for decades, marginal in hiring, publication, and judicial-nomination advice.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legal_academic_establishment, beneficiary,
    organized, generational, identity_locked, national).

% Bring claims asking courts to recognize rights not enumerated in the text or not previously enforced — privacy, reproductive autonomy, marriage equality, criminal-procedure protections. Their path to recognition runs through judicial elaboration rather than constitutional amendment, which is practically closed to them. They win when the values-gate admits their claim; they pay litigation costs, multi-year timelines, and doctrinal uncertainty either way.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, rights_claiming_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Enact policies through ordinary legislation that are then invalidated under judicially elaborated constitutional standards — school segregation regimes, abortion restrictions, sodomy statutes, death-penalty practices. Their remedy in principle is constitutional amendment, but supermajority requirements have made substantive amendment effectively unavailable for generations, so they absorb losses, regroup at elections, and contest judicial appointments instead.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, displaced_democratic_majorities, payer,
    organized, generational, constrained, national).

% Originalists and positivists who hold that this reading misunderstands what constitutional authority is. For decades their adherents were largely excluded from elite academic hiring, prestigious clerkships, and judicial shortlists. They organized separately — a dedicated legal network, alternative journals, their own nomination pipeline — and eventually captured substantial judicial power, at which point the orthodoxy's enforcement against them visibly weakened.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, rival_interpretive_schools, payer,
    organized, generational, constrained, national).

% Administer criminal justice, family law, education, and elections under floors and mandates set by federal judicial elaboration. Their policy space narrows as federal constitutional standards expand into areas they previously governed; their recourse is compliance litigation, lobbying over appointments, and occasional cooperative-federalism bargains.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_governments, payer,
    institutional, generational, constrained, national).

% Hold considered views about what the Constitution means, but outside courts those views have no legal force. Movements from abolition to gun rights to reproductive justice have claimed the Constitution directly; under this reading such claims are raw material — 'social attitudes and values' the courts consult — rather than constitutional meaning in their own right.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, popular_constitutionalist_publics, excluded,
    powerless, generational, trapped, national).

% Study the United States as an outlier: a very old, very short, very hard-to-amend constitution paired with an unusually powerful judiciary. They supply the external evidence on amendment difficulty and cross-national judicial-power variance against which the reading's functional claims can be checked.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a short, nearly unamendable eighteenth-century text administrable across centuries of technological, demographic, and moral change: doctrinal adaptation through adjudication supplies constitutional updating without requiring supermajority amendment for every adjustment.
% TRANSFER_FUNCTION: Moves constitutional change-authority from Article V amendment coalitions and sitting legislative majorities to the federal judiciary, and moves interpretive legitimacy from historical public meaning and popular constitutional claim-making to the professional elite culture that staffs the courts and the academy.
% ABSENT_VOICES: Popular constitutionalist publics — citizens whose understandings of the Constitution carry no legal weight unless a court ratifies them — and would-be Article V amendment proponents, for whom the formal channel is practically sealed. Both are procedurally outside the courtroom conversation where operative meaning is actually made.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the doctrinal edifice built on evolving standards — unenumerated privacy and autonomy rights, much of modern equal-protection and criminal-procedure law — would lose its legitimating frame; constitutional change would have to route through Article V or ordinary politics; the judiciary's role would contract toward enforcement of enacted text; and the legal academy's dominant paradigm would lose its object.
% FOUNDING_PROBLEM: The dead-hand problem: a brief, hard-to-amend founding text must govern a continental, technological, pluralistic society its authors could not foresee, and something must supply the updating function that Article V cannot.
% FOUNDING_PROBLEM_CORROBORATION: Historians and political scientists outside the benefiting institutions attest Article V's practical impossibility — only seventeen amendments since the Bill of Rights, none substantively transformative in decades, and the U.S. constitution is a cross-national outlier in amendment difficulty (comparative constitutional scholarship corroborates). Sibling-reading scholars concede the amendment difficulty but dispute that it licenses judicial substitution — they contest that the problem as posed has the judicial-supremacy answer built into it.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-18',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k3', 'max_tokens=32000,temperature=default,reasoning=max').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.48, 'kimi-k3', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text_authority__living_constitutionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Metrics are authored for the regime's actual operation on one shared grid (interval 0–30, roughly 1940→2025). Extractiveness peaks mid-to-late interval (0.52 at t=18, the Warren/Burger rights-expansion apex and its consolidation) and settles at 0.48 as the originalist insurgency recaptures judicial seats — judicial updating authority remains large but its throughput contracts at the margin. Suppression tracks professional-orthodoxy enforcement, not physical coercion: rising from 0.30 (post-1937 pluralism) to 0.58 (era when rival methodology was effectively disqualifying in elite hiring, clerkships, and nomination advice), then DECAYING to 0.50 as the rival schools built counter-institutions and won appointments — that decay is load-bearing evidence against a snare reading: exits were contested but never sealed. Theater is moderate (0.30): the doctrinal work is real adjudication, but the gap between 'we merely interpret the text' and the functional amendment-substitution the reading performs is a standing performative element, and it grew with the activism it covered. Accessibility_collapse is low-moderate (0.40) — alternative readings never collapsed; originalism survived as insurgency and returned to power. Resistance is substantial (0.55) — organized, sustained, and ultimately partially successful (confirmation wars, counter-networks, doctrinal retrenchment). Identity-lock note: both the judiciary (institutional identity fused with interpretive supremacy) and the academy (professional identity fused with the doctrinal-elaboration paradigm) are bound by institutional identity, not material dependence; if either identity frame broke — a court voluntarily accepting a diminished role, an academy treating methodology as plural — the enforcement mechanism would dissolve faster than any external challenge could achieve.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience the regime as fidelity-preserving coordination: the judiciary applies enduring principles to new facts; the academy elaborates doctrine; successful litigants receive recognition the amendment channel could never have delivered — rope-like from those seats. The payer seats experience enforced extraction: majorities watch enacted policy invalidated by standards no one ratified; rival schools spent decades as professionally disqualifying minorities; states govern under floors they did not enact; popular constitutionalists are told their understandings count only once a court says so. The same structure is the coordination from above and the extraction from below — the engine computes this divergence per seat from the declared roles, power, and exits; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain handles this constraint without overrides because roles and exits are already differentiated. The federal_judiciary sits at the beneficiary end: it administers the gate and the gain (change-authority, docket centrality, prestige) demonstrably accrues to it — which is why gain_flow names it rather than 'diffuse'. The academy is a clean beneficiary (collects paradigm rents without running enforcement). Rights_claiming_litigants are beneficiaries-with-costs: moderate power and constrained exit place them nearer symmetric than the institutional beneficiaries — their wins are real but gated and expensive. Displaced majorities and states carry high d (targets with sealed formal remedies); rival_interpretive_schools carry high d during the orthodoxy era and their partial capture of the judiciary late in the interval is exactly what the falling suppression series records. Popular constitutionalist publics are excluded rather than coordinated — their 'values' are inputs to the gate, not authorship of it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Article V's practical impossibility supplying constitutional updating) is externally corroborated and still real, but whether it licenses THIS remedy is contested — hence founding_problem_status: contested rather than live. The tangled_rope classification prevents two symmetric mislabelings: (a) the reading's flattering self-image as pure rope — adaptation is real, but so is the authority transfer and the decades of professional orthodoxy enforcement; (b) the rival schools' caricature as pure snare — the dead-hand coordination function is genuine, payer resistance was never successfully suppressed (the insurgency won), and the values-gate has at times tracked genuine moral correction (Brown) that the formal channel had failed to deliver. A claimed rope that computes extractive, or a claimed snare whose exits demonstrably stayed open, are both caught by the measurement surface rather than by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_reading_committer_structure,
    'This constraint instantiates one reading of the constitutional_text_authority kernel (living_constitutionalist_reading). How would the beneficiary/victim structure and extraction profile change under the sibling readings (originalist_reading, positivist_reading)?',
    'Compile and classify all three reading constraints side by side; compare per-seat classifications, victim sets, and extraction profiles across the family. The disagreement is located at two structural elements: (a) what legitimately changes constitutional meaning (contemporary values vs. ratification-era public meaning vs. formal enactment), and (b) which institution holds the change-authority (courts vs. Article V coalitions vs. enactment procedures).',
    'Each sibling redistributes the seats: under the originalist reading, this reading''s agenda-setter and beneficiary institutions become payers whose paradigm is displaced; under the positivist reading, moral-content litigants on all sides pay. The classification of THIS constraint is valid only for this reading; no averaging across readings is meaningful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_reading_committer_structure, conceptual, 'One reading of the constitutional text authority kernel; siblings redistribute the seats.').

omega_variable(
    whose_contemporary_values_gate,
    'When contemporary values gate permissible constitutional outcomes, whose values actually operate the gate — national public opinion, or the professional elite culture (judiciary, academy, bar) that staffs the interpretive institutions?',
    'Compare doctrinal shifts against longitudinal mass public-opinion data versus elite-opinion proxies (law-review consensus, bar-association positions, appellate-bar norms, clerkship-pipeline attitudes); the empirical literature on whether courts track mass or elite opinion directly addresses this.',
    'If the gate tracks mass opinion, part of the measured extraction is democratic responsiveness and payer-seat classifications soften; if it tracks elite opinion, extraction concentrates further on the agenda-setter seat, the transfer runs public-to-professional-class, and payer seats drift snare-ward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whose_contemporary_values_gate, empirical, 'Whether the values-gate is majoritarian or elite-operationalized.').

omega_variable(
    brown_amendment_or_recovery,
    'Does Brown v. Board (1954) — this reading''s canonical warrant — evidence that constitutional meaning legitimately changed without Article V, or did it recover meaning the Fourteenth Amendment always carried, as sibling readings contend?',
    'Historical research on Reconstruction-era public understanding of the Fourteenth Amendment and segregated schooling. This is a live and possibly irresolvable historiographic contest — the originalist tradition itself splits on Brown, which is diagnostic.',
    'If Brown changed rather than recovered meaning, the extraction of Article V authority is confirmed and the reading''s legitimacy rests on the normative case for judicial substitution; if Brown recovered original meaning, the canonical case fails to distinguish this reading from its originalist sibling and part of the reading''s legitimacy narrative is theater rather than warrant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brown_amendment_or_recovery, conceptual, 'Whether the canonical warrant case distinguishes this reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t6, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement_basis(cons_tr_t6, observed).
narrative_ontology:measurement(cons_tr_t12, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(cons_tr_t12, observed).
narrative_ontology:measurement(cons_tr_t18, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 18, 0.33).
narrative_ontology:measurement_basis(cons_tr_t18, observed).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement_basis(cons_tr_t24, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement_basis(cons_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t6, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 6, 0.38).
narrative_ontology:measurement_basis(cons_be_t6, observed).
narrative_ontology:measurement(cons_be_t12, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 12, 0.46).
narrative_ontology:measurement_basis(cons_be_t12, observed).
narrative_ontology:measurement(cons_be_t18, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 18, 0.52).
narrative_ontology:measurement_basis(cons_be_t18, observed).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 24, 0.5).
narrative_ontology:measurement_basis(cons_be_t24, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement_basis(cons_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t6, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 6, 0.4).
narrative_ontology:measurement_basis(cons_su_t6, observed).
narrative_ontology:measurement(cons_su_t12, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(cons_su_t12, observed).
narrative_ontology:measurement(cons_su_t18, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 18, 0.58).
narrative_ontology:measurement_basis(cons_su_t18, observed).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement_basis(cons_su_t24, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(cons_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition of the colloquial label 'constitutional text authority' into three rival readings, per ε-invariance. This file models the living_constitutionalist_reading: the judiciary holds change-authority and the values-gate is operationalized by professional elite culture, yielding extraction concentrated on displaced majorities, rival schools, and states. The originalist_reading relocates change-authority in Article V and historical meaning — its ε and victim set differ (the living paradigm's institutions become payers under its ascendancy). The positivist_reading relocates validity in enactment procedures and severs law from moral content — its ε differs again (moral-content litigants pay on all sides). This reading influences both siblings: its institutional dominance set the legitimacy conditions against which the originalist insurgency organized and against which positivist accounts must define themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
