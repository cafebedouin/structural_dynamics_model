% ============================================================================
% CONSTRAINT STORY: second_amendment_scope__civic_right_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_scope__civic_right_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: second_amendment_scope__civic_right_reading
 *   human_readable: Second Amendment Individual Right Conditioned on Civic Militia Participation
 *   domain: constitutional_law/rights
 *
 * SUMMARY:
 *   The Second Amendment's text contains an irreducible tension: it speaks
 *   both to militia (collective defense) and to individual rights. The
 *   civic-right reading resolves this tension by making militia participation
 *   the condition that legitimates individual firearm ownership. Under this
 *   reading, the right is genuinely individual — one may own and carry — but
 *   only for those who accept the civic duty the militia participation
 *   entails. This creates a tangled structure: the constraint coordinates
 *   individual liberty with state preparedness needs (the coordination
 *   function) while simultaneously creating a bifurcated population — those
 *   eligible for the conditional right and those outside the militia
 *   framework (the extraction asymmetry). The claim/metric independence is
 *   operative here: the reading claims TANGLED ROPE (genuine coordination
 *   function + asymmetric extraction), and the metrics describe moderate
 *   extractiveness with required active enforcement, validating the claim
 *   structurally.
 *
 * KEY AGENTS:
 *   - militia_eligible_citizens: individuals meeting state-defined militia criteria — gain explicit constitutional protection through civic-duty conditioning
 *   - non_militia_populations: those outside militia frameworks (physically unable, legally excluded, ideologically opposed to the duty frame) — bear asymmetric loss of access
 *   - state_militia_authority: the institutional seat setting militia eligibility and readiness standards — benefits from gating the right to state-defined civic duty
 *   - individual_rights_advocates: organized scholars and civil liberties groups arguing for unconditional individual rights — excluded from this reading's framework
 *   - collective_authority_defenders: scholars arguing the amendment protects only state militia authority, not individual rights — also excluded; their core premise contradicts the civic reading
 *   - federal_courts: adjudicate between readings; their decisions determine which structure becomes enforceable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, 0.48).
domain_priors:suppression_score(second_amendment_scope__civic_right_reading, 0.31).
domain_priors:theater_ratio(second_amendment_scope__civic_right_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(second_amendment_scope__civic_right_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_scope__civic_right_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_scope__civic_right_reading, "Second Amendment Individual Right Conditioned on Civic Militia Participation").
narrative_ontology:topic_domain(second_amendment_scope__civic_right_reading, "constitutional_law/rights").

domain_priors:requires_active_enforcement(second_amendment_scope__civic_right_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_scope__civic_right_reading, '941cdd9b-639c-4fe2-b91b-1d1edca436d9').
narrative_ontology:cs_kernel_codification('941cdd9b-639c-4fe2-b91b-1d1edca436d9', fixed_text).
narrative_ontology:cs_authority_grounding('941cdd9b-639c-4fe2-b91b-1d1edca436d9', lineage).
narrative_ontology:cs_interpretation_layer_present('941cdd9b-639c-4fe2-b91b-1d1edca436d9').
narrative_ontology:cs_reading_relation('941cdd9b-639c-4fe2-b91b-1d1edca436d9', second_amendment_scope__individual_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('941cdd9b-639c-4fe2-b91b-1d1edca436d9', second_amendment_scope__collective_right_reading, forecloses).
narrative_ontology:cs_axiom('941cdd9b-639c-4fe2-b91b-1d1edca436d9', foundational, militia_participation_conditions_individual_right).
narrative_ontology:cs_axiom_status(militia_participation_conditions_individual_right, holdable).
narrative_ontology:cs_axiom_grounding('941cdd9b-639c-4fe2-b91b-1d1edca436d9', militia_participation_conditions_individual_right, empirically_contingent).
narrative_ontology:cs_axiom('941cdd9b-639c-4fe2-b91b-1d1edca436d9', secondary, civic_duty_legitimates_constitutional_rights).
narrative_ontology:cs_axiom_status(civic_duty_legitimates_constitutional_rights, holdable).
narrative_ontology:cs_axiom_grounding('941cdd9b-639c-4fe2-b91b-1d1edca436d9', civic_duty_legitimates_constitutional_rights, deontological).
narrative_ontology:cs_reference_frame('941cdd9b-639c-4fe2-b91b-1d1edca436d9', civic_militia_condition_binding).
narrative_ontology:cs_drift_state('941cdd9b-639c-4fe2-b91b-1d1edca436d9', contemporary_post_heller_doctrine, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('941cdd9b-639c-4fe2-b91b-1d1edca436d9', '2026-06-12T14:32:18Z').
narrative_ontology:cs_kernel_id(second_amendment_scope__civic_right_reading, second_amendment_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_scope__civic_right_reading, militia_eligible_citizens).
narrative_ontology:constraint_victim(second_amendment_scope__civic_right_reading, non_militia_populations).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_republicanism_doctrine).
narrative_ontology:constraint_vindicates(second_amendment_scope__civic_right_reading, civic_participation_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals meeting state-defined militia participation criteria (age, citizenship, training readiness, physical ability, legal status) receive explicit Second Amendment protection. Under the civic-right reading, their ownership right is grounded in their status as potential state defense assets. They benefit from constitutional legitimacy that frames their right as tied to civic duty, which they view as consistent with their identity as citizens. They can exercise the right for personal purposes (self-defense, recreation) while their eligibility also satisfies the state's militia readiness interest. For those who accept the civic-duty frame, the condition is not experienced as extraction — it is the basis of the right's legitimacy.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, militia_eligible_citizens, beneficiary,
    organized, generational, mobile, national).

% Individuals outside militia-participation frameworks bear the extraction cost of the conditional right. This category includes: those age-excluded (juveniles, elderly), those legally excluded (convicted felons, non-citizens, those with certain disabilities), those unable to meet training requirements, and those ideologically opposed to civic-duty conditioning. Under the civic-right reading, they cannot claim the conditional right. They may argue for an alternative constitutional basis for firearm rights, but must do so outside the Second Amendment frame, or advocate for a different reading (individual-right or collective readings). Their access to the protected right is contingent on criteria they may not control and may not accept. They bear the cost of exclusion without obvious choice.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, non_militia_populations, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_scope__civic_right_reading, non_militia_populations, excluded).

% The state (through legislature, courts, National Guard administration, and emergency management) controls the definition of militia eligibility and readiness standards. The civic-right reading grants the state regulatory authority over who qualifies for the conditional right. The state benefits from a constitutional framework that ties individual firearm rights to state-defined civil preparedness roles. The state can use militia standards to shape who may own firearms, and it has incentive to maintain those standards to sustain the legitimacy of its authority over both the militia and the right. The state's interest aligns with keeping the militia-condition requirement visible and enforceable.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, state_militia_authority, agenda_setter,
    institutional, generational, analytical, national).

% Civil liberties organizations, constitutional scholars, and advocacy groups arguing that the Second Amendment protects an unconditional individual right are excluded from the civic-right reading's framework. They contest the militia condition itself, arguing that conditioning a fundamental right on state-defined civic duty is inconsistent with individual liberty principles. They are not present in the conversation this reading sets up; their absence is structural — the reading's core premise (militia participation IS the condition) contradicts their core premise (the condition is not binding). They would object that the civic reading improperly subordinates individual liberty to state power. Their resistance is high (0.58) because they have scholarly resources and institutional presence, but they are excluded from this reading's dialogue.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, individual_rights_advocates, excluded,
    organized, biographical, constrained, national).

% Constitutional scholars and some historical researchers who argue that the Second Amendment protects ONLY state authority to maintain militia institutions (not individual ownership rights) are also excluded. This reading grants an individual component; the collective-authority reading denies it. The two positions cannot coexist on the same amendment — they are not just different emphases but contrary claims about the right's scope. The collective-authority advocates would argue that the civic reading improperly grafts an individual right onto language that concerns only militia. Their exclusion is due to logical opposition, not institutional powerlessness.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, collective_authority_defenders, excluded,
    organized, biographical, constrained, national).

% Federal courts, especially the Supreme Court, are the institutional seat that interprets the Second Amendment and determines which reading becomes enforceable constitutional doctrine. Courts see the text as containing the tension between militia and individual components. The civic-right reading presents one coherent resolution of that tension. Courts can adopt, reject, or modify any reading. As observers, courts are not beneficiaries or payers of the constraint itself, but their decisions determine which constraint becomes law. The current doctrine (post-Heller 2008) leans toward an individual-right reading without explicit militia conditioning, which means federal courts have currently rejected the civic-right reading as binding law, though some lower courts and scholars still maintain it.
narrative_ontology:constraint_stakeholder(second_amendment_scope__civic_right_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_scope__civic_right_reading, state_militia_authority).
narrative_ontology:fixing_cost_class(second_amendment_scope__civic_right_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The reading coordinates individual firearm rights with state preparedness needs by making civic militia participation the legitimate basis for individual ownership. It solves a structural alignment problem: how can the Second Amendment protect both militia (collective) and individual rights without the individual component making the militia clause superfluous? By conditioning the individual right on militia eligibility, the reading makes the amendment's two clauses structurally coherent — militia participation is both the public purpose the amendment serves AND the credential that qualifies one for the protected right. Individual liberty and collective security are aligned, not opposed.
% TRANSFER_FUNCTION: The constraint transfers regulatory authority from individual demand (who wants to own a firearm) to state-gated criteria (who meets militia-participation standards). It also transfers access asymmetrically: militia-eligible individuals gain explicit constitutional protection; non-militia populations must either satisfy militia criteria or abandon the claim. The transfer is from individual choice to conditional civic qualification, with the state as the gatekeeper.
% ABSENT_VOICES: Those who argue the Second Amendment protects an unconditional individual right (not contingent on militia service) would object if present — they are excluded by the reading's framing because the civic reading explicitly conditions the right on civic duty, which their premise rejects. Those who argue the amendment protects only state militia authority (not individual rights at all) would also object — they are excluded because the civic reading affirms an individual component they deny. The reading's dialogue structure forecloses both sibling positions from entry.
% DISAPPEARANCE_RATIONALE: If the Second Amendment were read as protecting an unconditional individual right (the individual reading) instead of a conditional civic right, or if the militia condition were removed entirely, constitutional doctrine would shift. Firearm access would decouple from militia participation criteria; state regulatory authority over the right would be constrained; litigation would follow different doctrinal pathways; and the justification for differential access between militia-eligible and non-militia populations would collapse. Federal law on firearm rights would reorganize around different constitutional principles.
% FOUNDING_PROBLEM: The founding problem is the structural tension in the Second Amendment text itself: 'A well regulated Militia, being necessary to the security of a free State, the right of the people to keep and bear Arms, shall not be infringed.' The text specifies both a militia (collective) component and a 'right of the people' (individual) component without explicitly resolving their relationship. The civic-right reading resolves this by making militia participation the condition that legitimates the individual right — the clause structure becomes: [because militia is necessary], [individuals qualified for militia service get the right]. The founding problem is: how to read these clauses as coherent rather than contradictory.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is directly evidenced by the constitutional text and by historians of the founding era, who document that framers debated the militia-clause's relationship to individual rights. Some founding-era sources (e.g., certain state constitution precursors, Federalist Papers commentary) suggest militia participation was understood as the legitimate basis for the right. However, other sources (e.g., founding-era natural rights theory, some state constitutions' separate militia and individual-rights clauses) suggest an individual right existed independently of militia duty. Modern scholars from outside the benefiting parties (academic historians, constitutional law professors without ideological alignment to gun-rights or gun-control movements) dispute the binding nature of the militia condition. The National Archives and Library of Congress historical texts support the contested status — the original intent is genuinely underdetermined by the documentary record.
narrative_ontology:disappearance_verdict(second_amendment_scope__civic_right_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_scope__civic_right_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_scope__civic_right_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(second_amendment_scope__civic_right_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_scope__civic_right_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_scope__civic_right_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_scope__civic_right_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint creates genuine coordination (aligning individual rights with state preparedness) but also creates a conditional gate that beneficiaries (militia-eligible) do not have to cross while non-militia populations do. The gate is the extraction mechanism: those who meet civic-duty criteria qualify automatically; those who do not must either satisfy new criteria or abandon the claim. Suppression is lower (0.31) because the constraint does not require active coercion to maintain — it operates through conditional eligibility, a lighter-touch mechanism than direct prohibition. Theater is modest (0.22) because the militia-participation frame is genuinely part of the constitutional argument, though debate over whether it is a binding condition (essential to the right) or merely a reason (explaining but not limiting the right) introduces some theatrical performance around the coordination claim. The measurement series shows extractiveness and suppression rising modestly through the interval as state militia requirements tighten and the conditional gate becomes more administratively specified — a drift toward stronger gating. The measurements share one time grid so every metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   From the militia-eligible beneficiary seat, the constraint appears as genuine civic coordination: a right that rewards civic duty and aligns individual liberty with national preparedness. From the non-militia population seat, the same structure appears as conditional exclusion: a right artificially limited to those meeting state-set criteria, with no intrinsic reason why non-militia individuals should lack it. Federal courts, as observers, see a constitutional text that admits both readings; the courts' role is to arbitrate which reading becomes binding law. The engine computes these divergent effective types from the structural data — no tuning required.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality diverges dramatically between seats because the constraint's structural relationship differs by power and militia status. Militia-eligible citizens (organized power, mobile exit) experience low directionality (d near 0.2–0.3): they are beneficiaries receiving explicit constitutional protection; the constraint subsidizes their firearm access. State militia authority (institutional power) experiences symmetric directionality (d near 0.5): it benefits from legitimacy and compliance authority but must maintain the militia infrastructure the right depends on. Non-militia populations (moderate power, constrained exit) experience high directionality (d near 0.7–0.8): they bear the cost of exclusion or must argue for the right outside the militia frame. The constraint's effective extraction is therefore asymmetric by militia status — the engine computes this from the beneficiary/victim declarations and exit differentials.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids simple misclassification because both the coordination function and the extraction asymmetry are visible in the structure. A pure-rope reading (only coordination, no asymmetry) would falsely erase the non-militia population's differential access. A pure-snare reading (only extraction, no coordination) would falsely erase the genuine alignment of militia participation with defense needs. Tangled-rope classification holds both: the militia-participation condition is a real coordination solution to the founding problem (how to justify an individual right within a militia framing) AND a mechanism that creates asymmetric extraction (those outside the militia framework lose access). The classification prevents either side of the dispute from projecting its preferred frame onto the whole structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    militia_condition_binding,
    'Is the militia clause in the Second Amendment a binding CONDITION on individual rights (one must be militia-eligible to qualify), or merely an explanatory REASON that does not limit the right to militia contexts?',
    'Originalist historical research on founding-era understanding of the text''s grammar and legal theory; comparative analysis of how founding-era legal texts used conditional vs. explanatory clauses; analysis of state ratification debates and contemporary constitutional interpretation.',
    'If condition: the civic-right reading is structurally correct, and non-militia populations can be validly excluded from Second Amendment protection. If reason-only: the individual reading is correct, and the militia clause explains the right''s public purpose but does not limit its scope, invalidating the extraction asymmetry in the civic reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_condition_binding, empirical, 'Whether militia participation is a binding condition or an explanatory reason for the right.').

omega_variable(
    founding_problem_resolution_adequacy,
    'Does conditioning the right on militia participation genuinely resolve the founding problem of aligning individual liberty with collective security, or does it merely subordinate one to the other without resolution?',
    'Analysis of whether militia-eligible individuals in practice exercise the right for both personal and state-service purposes, or whether the condition operates as a rationing device; historical analysis of founding-era practice and stated intent; comparative constitutional theory on how other democracies align individual rights with collective-duty frameworks.',
    'If genuine resolution: the tangled-rope classification holds and the constraint serves a real coordination function. If subordination without resolution: the constraint is better classified as a snare that uses coordination language to mask extraction, and the extraction asymmetry is primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_resolution_adequacy, conceptual, 'Whether the militia condition solves a real coordination problem or masks extraction.').

omega_variable(
    exclusion_basis_legitimacy,
    'What criteria define militia eligibility in this reading, and are those criteria themselves contested or stable? Do they track genuine preparedness capacity, or do they encode other social divisions?',
    'Empirical analysis of how states historically defined militia membership (age, property, race, gender, disability, criminal record); analysis of whether those definitions tracked security-relevant capacity or encoded exclusions for other reasons; legislative history and court decisions addressing militia criteria.',
    'If eligibility criteria track genuine preparedness: the condition is not arbitrary and the extraction is justified by the coordination problem. If criteria encode social exclusions: the constraint is using the militia frame to legitimate extraction based on status, making it more snare-like than tangled-rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exclusion_basis_legitimacy, empirical, 'Whether militia eligibility criteria track preparedness or encode other exclusions.').

omega_variable(
    reading_interpretive_authority,
    'Which interpretive tradition (originalist, living constitutionalist, civic republican) currently holds interpretive authority over the Second Amendment, and does that authority''s decision method favor the civic-right reading?',
    'Analysis of dominant Supreme Court doctrine (post-Heller 2008 and District of Columbia v. Heller precedent); analysis of how different courts'' interpretive methods treat the militia clause; tracking of shifts in interpretive authority and how each tradition handles the tension between militia and individual components.',
    'If originalism holds sway and originalists read the condition as binding: the civic reading gains doctrinal authority. If living constitutionalism holds sway and courts read the militia clause as historical context rather than condition: the individual reading gains authority. The constraint''s actual enforceability depends on which reading''s authority structure wins.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_interpretive_authority, empirical, 'Which interpretive tradition controls Second Amendment doctrine and what do they say about militia conditioning.').

omega_variable(
    civic_duty_legitimacy_frame,
    'Is the civic-republicanism doctrine (that rights depend on civic participation duties) a legitimate and sustained constitutional principle, or is it a contested and declining frame that the reading imports into the Second Amendment?',
    'Historical analysis of civic-republican theory in founding-era political thought; analysis of how other constitutional rights are framed (are voting, speech, and assembly also conditioned on civic duty, or are they framed as unconditional); analysis of contemporary constitutional scholars'' acceptance or rejection of civic-duty conditioning.',
    'If civic republicanism is a strong and consistent constitutional frame: the militia-condition reading fits a broader pattern of duty-based rights and is not an anomaly. If it is a declining or contested frame: the civic reading represents an attempt to import a particular political theory into the amendment''s text, making it a reading-specific rather than amendment-inherent feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civic_duty_legitimacy_frame, conceptual, 'Whether civic-republicanism is a legitimate and sustained constitutional frame or a contested theory imported into this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_scope__civic_right_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(seco_tr_t0, second_amendment_scope__civic_right_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(seco_tr_t0, observed).
narrative_ontology:measurement(seco_tr_t8, second_amendment_scope__civic_right_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(seco_tr_t8, observed).
narrative_ontology:measurement(seco_tr_t16, second_amendment_scope__civic_right_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement_basis(seco_tr_t16, observed).
narrative_ontology:measurement(seco_tr_t24, second_amendment_scope__civic_right_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(seco_tr_t24, observed).
narrative_ontology:measurement(seco_tr_t32, second_amendment_scope__civic_right_reading, theater_ratio, 32, 0.22).
narrative_ontology:measurement_basis(seco_tr_t32, observed).
narrative_ontology:measurement(seco_tr_t40, second_amendment_scope__civic_right_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(seco_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(seco_be_t0, second_amendment_scope__civic_right_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(seco_be_t0, observed).
narrative_ontology:measurement(seco_be_t8, second_amendment_scope__civic_right_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement_basis(seco_be_t8, observed).
narrative_ontology:measurement(seco_be_t16, second_amendment_scope__civic_right_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement_basis(seco_be_t16, observed).
narrative_ontology:measurement(seco_be_t24, second_amendment_scope__civic_right_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement_basis(seco_be_t24, observed).
narrative_ontology:measurement(seco_be_t32, second_amendment_scope__civic_right_reading, base_extractiveness, 32, 0.48).
narrative_ontology:measurement_basis(seco_be_t32, observed).
narrative_ontology:measurement(seco_be_t40, second_amendment_scope__civic_right_reading, base_extractiveness, 40, 0.48).
narrative_ontology:measurement_basis(seco_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(seco_su_t0, second_amendment_scope__civic_right_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement_basis(seco_su_t0, observed).
narrative_ontology:measurement(seco_su_t8, second_amendment_scope__civic_right_reading, suppression_requirement, 8, 0.22).
narrative_ontology:measurement_basis(seco_su_t8, observed).
narrative_ontology:measurement(seco_su_t16, second_amendment_scope__civic_right_reading, suppression_requirement, 16, 0.25).
narrative_ontology:measurement_basis(seco_su_t16, observed).
narrative_ontology:measurement(seco_su_t24, second_amendment_scope__civic_right_reading, suppression_requirement, 24, 0.29).
narrative_ontology:measurement_basis(seco_su_t24, observed).
narrative_ontology:measurement(seco_su_t32, second_amendment_scope__civic_right_reading, suppression_requirement, 32, 0.3).
narrative_ontology:measurement_basis(seco_su_t32, observed).
narrative_ontology:measurement(seco_su_t40, second_amendment_scope__civic_right_reading, suppression_requirement, 40, 0.31).
narrative_ontology:measurement_basis(seco_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_scope__civic_right_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(second_amendment_scope__civic_right_reading, 0.12).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_scope__civic_right_reading, second_amendment_scope__collective_right_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_scope kernel generates three distinct constraint stories with ε values that diverge substantially. This story (civic_right_reading) treats the militia clause as a binding condition (ε moderate, tangled_rope). The individual_right_reading treats militia mention as explanatory only (ε lower, more rope-like). The collective_right_reading denies an individual component entirely (ε depends on whether it is treated as coordinate or pure state power). The stories are linked because each reading's authority claim explicitly contests the others' validity within the same constitutional text. A court decision favoring one reading affects the effective classification of the others by changing their doctrinal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, powerless, 0.75).
constraint_indexing:directionality_override(second_amendment_scope__civic_right_reading, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
