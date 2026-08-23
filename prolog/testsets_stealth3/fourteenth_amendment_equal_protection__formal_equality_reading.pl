% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__formal_equality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__formal_equality_reading, []).

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
 *   constraint_id: fourteenth_amendment_equal_protection__formal_equality_reading
 *   human_readable: Equal Protection — Formal Equality Reading (Anti-Classification Command)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the Fourteenth Amendment's Equal Protection guarantee; this file carries
 *   the formal_equality_reading, under which the guarantee prohibits explicit
 *   state racial and similar status classification absent a compelling
 *   justification. The sibling file,
 *   fourteenth_amendment_equal_protection__anti_caste_reading, instantiates
 *   the anti-caste reading, under which the same guarantee requires active
 *   dismantling of hierarchy. Per the epsilon-referent rule, both stories
 *   assess the SAME standing arrangement — state action under the
 *   formal-equality doctrine as it actually operates — and differ only in the
 *   lights they assess it by. This reading counts the persistence of
 *   structural inequality as pre-constitutional background rather than as the
 *   rule's own output, so it authors a low-to-moderate epsilon (0.38) where
 *   the sibling authors a high one. The claim/metric split is deliberate: the
 *   constraint is CLAIMED as tangled_rope because it pairs a genuine
 *   coordination function with a declared victim set and active judicial
 *   enforcement, while the metrics describe what this reading's own lights
 *   concede — bounded but real, and slowly accumulating, costs concentrated
 *   on remedial capacity.
 *
 * KEY AGENTS:
 *   - - racial_minority_citizens: Dual-positioned class seat (organized/trapped) — shielded from explicit state discrimination while losing race-conscious corrective tools through the same rule
 *   - - majority_group_applicants: Primary beneficiary (moderate/mobile) — competes without classification weighing against them; receives reallocated positions and awards
 *   - - colorblind_constitutionalism_advocates: Ideological beneficiary (organized/identity_locked) — collects doctrinal vindication; exit fused with the principle itself
 *   - - underrepresented_minority_students: Primary target (powerless/constrained) — loses access pathways; leverage exercised through civil-rights coalitions
 *   - - integration_planning_school_districts: Institutional target (institutional/constrained) — voluntary race-conscious assignment plans invalidated
 *   - - minority_owned_contractors: Target (moderate/constrained) — set-aside programs struck down; exposed to incumbent-network bidding
 *   - - federal_reviewing_court: Agenda-setter (institutional/constrained) — administers strict scrutiny and owns the compelling-justification standard
 *   - - anti_subordination_scholars: Excluded voice (organized/mobile) — argues the rival reading from outside the controlling interpretive coalition
 *   - - constitutional_law_scholars: Analytical observer (analytical/analytical) — maps the structure without collecting or paying
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__formal_equality_reading, 0.4).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__formal_equality_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__formal_equality_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__formal_equality_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__formal_equality_reading, "Equal Protection — Formal Equality Reading (Anti-Classification Command)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__formal_equality_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__formal_equality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__formal_equality_reading, '75348553-ee3d-4ac2-a499-e37943b166ab').
narrative_ontology:cs_kernel_codification('75348553-ee3d-4ac2-a499-e37943b166ab', fixed_text).
narrative_ontology:cs_authority_grounding('75348553-ee3d-4ac2-a499-e37943b166ab', lineage).
narrative_ontology:cs_interpretation_layer_present('75348553-ee3d-4ac2-a499-e37943b166ab').
narrative_ontology:cs_reading_relation('75348553-ee3d-4ac2-a499-e37943b166ab', fourteenth_amendment_equal_protection__anti_caste_reading, forecloses).
narrative_ontology:cs_axiom('75348553-ee3d-4ac2-a499-e37943b166ab', foundational, state_racial_classification_presumptively_invalid).
narrative_ontology:cs_axiom_status(state_racial_classification_presumptively_invalid, holdable).
narrative_ontology:cs_axiom_grounding('75348553-ee3d-4ac2-a499-e37943b166ab', state_racial_classification_presumptively_invalid, deontological).
narrative_ontology:cs_axiom('75348553-ee3d-4ac2-a499-e37943b166ab', foundational, structural_inequality_is_preconstitutional_background).
narrative_ontology:cs_axiom_status(structural_inequality_is_preconstitutional_background, holdable).
narrative_ontology:cs_axiom_grounding('75348553-ee3d-4ac2-a499-e37943b166ab', structural_inequality_is_preconstitutional_background, conventional).
narrative_ontology:cs_reference_frame('75348553-ee3d-4ac2-a499-e37943b166ab', colorblind_civic_equality_baseline).
narrative_ontology:cs_drift_state('75348553-ee3d-4ac2-a499-e37943b166ab', contemporary_post_sffa, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('75348553-ee3d-4ac2-a499-e37943b166ab', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_citizens).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism_advocates).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, underrepresented_minority_students).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, integration_planning_school_districts).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, minority_owned_contractors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_citizens).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, colorblindness_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, strict_scrutiny_framework).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__formal_equality_reading, state_impartiality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Citizens exposed to state decisions that could sort them by race. The rule guarantees them impartial treatment in voting, policing, public services, and jury selection, and it was the instrument that dismantled legally enforced segregation. The same rule also removes race-conscious tools aimed at their communities' accumulated disadvantages: admissions pathways, contracting set-asides, and school-integration plans. Exit from the rule is unavailable — it travels with citizenship and jurisdiction.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_citizens, beneficiary,
    organized, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(fourteenth_amendment_equal_protection__formal_equality_reading, racial_minority_citizens, payer).

% Applicants to public universities, bidders on public contracts, and candidates for programs where a racial criterion would weigh against them. They receive the positions, awards, and placements that classification-based allocation would have directed elsewhere, and they can turn to private or out-of-state alternatives when public options disappoint.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants, beneficiary,
    moderate, biographical, mobile, national).

% A legal and political movement built around the neutrality principle. It staffs litigation campaigns, judicial nominations, and state-level replication efforts such as ballot initiatives banning affirmative action. Its members' professional standing and ideological identity are fused with the principle; abandoning it would dissolve the movement's reason for being. It collects doctrinal victories — most recently in admissions — without administering the rule day to day.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, colorblind_constitutionalism_advocates, beneficiary,
    organized, generational, identity_locked, national).

% Students seeking entry to selective public institutions whose race-conscious admissions pathways have been struck down. Remaining routes — socioeconomic proxies, outreach pipelines, private and historically black institutions — are narrower and depend on resources many applicants lack. Their main lever is collective: civil-rights organizations litigate and lobby on their behalf, but the students themselves hold no institutional power over the rule.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, underrepresented_minority_students, payer,
    powerless, biographical, constrained, national).

% Districts that voluntarily adopted race-conscious student-assignment plans to counteract residential segregation. The reviewing court invalidated those plans, leaving income-based assignment, magnet themes, and boundary redraws as substitutes with weaker integrative effect. The districts cannot opt out of constitutional review; they operate whatever tools the rule leaves open.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, integration_planning_school_districts, payer,
    institutional, generational, constrained, regional).

% Firms that lost municipal and state set-aside programs after the reviewing court began demanding firm-by-firm proof of specific past discrimination. They now bid in nominally open competitions where established networks, bonding requirements, and cumulative disadvantage operate against them. Leaving public contracting is possible but forfeits their principal market.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, minority_owned_contractors, payer,
    moderate, biographical, constrained, national).

% The court that administers the rule: it defines what counts as a suspect classification, sets the compelling-justification standard, and strikes down state action that fails it. It is bound by its own precedents and by legitimacy costs that make abrupt reversal expensive. It gains no material revenue from the rule; its return is doctrinal control.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, federal_reviewing_court, agenda_setter,
    institutional, generational, constrained, national).

% Legal academics, dissenting judges, and advocacy lawyers who argue that the rule should require active dismantling of hierarchy rather than neutrality alone. They publish, dissent, and litigate at the margins but hold no seat in the coalition that currently controls the rule's administration.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, anti_subordination_scholars, excluded,
    organized, generational, mobile, national).

% Academic observers who map the doctrine's structure, trace its history, and compare it with other constitutional systems. They neither collect from nor bear the rule's costs.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__formal_equality_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__formal_equality_reading, majority_group_applicants).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__formal_equality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Gives every state actor one uniform, administrable rule of civic dealing: distribute no burden or benefit by explicit racial or similar status classification except under the most demanding showing of necessity. It solves a real multi-jurisdiction coordination problem — preventing a race-to-caste among states and giving citizens a portable guarantee of impartial state treatment — and supplies a bright-line test that lower courts, agencies, and legislatures can apply without open-ended balancing.
% TRANSFER_FUNCTION: Moves legal security from state institutions, which surrender the discretion to classify, to individual citizens, who receive an enforceable guarantee of impartial treatment. In its contemporary operation it also moves concrete allocations — admissions offers, contracts, school placements — away from the intended beneficiaries of race-conscious programs and toward applicants whom those classifications would have disfavored.
% ABSENT_VOICES: Anti-subordination advocates, the intended beneficiaries of struck-down programs, and communities still living with the residue of state-enforced exclusion would object that the rule freezes hierarchy in place; they speak from dissenting opinions, law-school scholarship, and advocacy filings rather than from the controlling interpretive seat. Their absence from the governing coalition is part of how the current reading holds.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, state governments would immediately regain the discretion to classify: some would reinstate exclusionary or majoritarian classifications, others would launch race-conscious remediation, and every allocation currently flowing through race-neutral channels would be renegotiated. The parties dispute the direction of the rearrangement — the colorblind camp expects little loss since statutes cover most protection; the anti-caste camp expects rapid re-entrenchment of hierarchy — but nobody expects the world to stay put.
% FOUNDING_PROBLEM: State-enforced caste: the Black Codes, and later Jim Crow, made state law itself the instrument of racial subordination. The Fourteenth Amendment was written to make it constitutionally impossible for a state to stamp a badge of servitude or caste on its citizens.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem itself is corroborated outside any benefiting party by the Reconstruction congressional record, the freedmen's-bureau correspondence, and the statutory text of the Black Codes it answered. Its present status is attested along the same fault line as the kernel contest: the colorblind camp and the reviewing court's current majority attest the problem is solved and the rule now guards against new caste-making by any race; civil-rights litigators, dissenting justices, and anti-subordination scholarship attest it survives in structural form. No seat outside the dispute is indifferent.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__formal_equality_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__formal_equality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__formal_equality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__formal_equality_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).
:- end_tests(fourteenth_amendment_equal_protection__formal_equality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon sits at 0.38 by this reading's own lights: the protective core (no state caste) is the rule's dominant product, the compelling-justification valve bounds the burden on remedial classification, and the costs the reading concedes — struck-down set-asides, invalidated assignment plans, narrowed admissions pathways — are real but concentrated. Suppression is 0.40: compliance is mandatory and non-opt-outable, but enforcement has normalized (see the falling suppression_requirement series) and the valve keeps the prohibition from being absolute. Theater is 0.28 and rising: early-interval operation was almost entirely functional (dismantling legally enforced segregation); late-interval operation increasingly consists of declaring neutrality amid disparity the rule declines to count. Accessibility_collapse is 0.58 — race-neutral substitutes exist but are weaker and partially scrutinized, and the valve remains formally open. Resistance is 0.55 — sustained doctrinal, scholarly, and political contestation rather than passive acquiescence. All three series share one eight-point grid (t0=1954 Brown, t70=2024 post-SFFA); the scissors pattern — rising extractiveness against falling suppression_requirement — records enforcement normalizing while foreclosure costs accumulate. Identity-lock note: the colorblind movement's exit is fused with its principle, so its seat stays locked regardless of doctrinal weather. Coalition note: the student seat is individually powerless but converts numbers into leverage through organized civil-rights litigation.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural data. The agenda-setting court experiences the rule as its own craft: a manageable standard it owns and refines. Beneficiary seats — disfavored applicants, the colorblind movement — experience protection and vindication. Payer seats — students, districts, contractors — experience the same rule as a wall placed in front of remedies they can name precisely. The dual-positioned citizen seat is the sharpest divergence: one sentence shields the same population from hostile classification and strips it of corrective tools, and whether that seat computes net-beneficiary or net-target decides whether the aggregate looks rope-like or snare-flavored. The engine derives this per seat from role, power, and exit data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the low-d seats: majority_group_applicants (mobile exit pushes them toward the beneficiary pole), colorblind_constitutionalism_advocates (identity_locked but collecting vindication), and racial_minority_citizens in their protective capacity. Victim declarations drive the high-d seats: underrepresented_minority_students (constrained exit), integration_planning_school_districts and minority_owned_contractors (institutional and constrained). The court sits near the beneficiary end as administrator without material receipts. No directionality_overrides are authored: the derivation chain separates the two moderate-power seats by exit options (mobile versus constrained), separates the two institutional seats by role (agenda_setter versus payer), and the dual-positioned citizen seat is carried by its secondary_role for per-seat computation — overriding it would flatten exactly the divergence the corpus exists to measure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state-enforced caste — is resolved in its paradigmatic form and contested in its structural form, so the mandate has transformed rather than died: the rule that once dismantled legal segregation now chiefly polices remedial classification. Mandatrophy is therefore NOT declared resolved. The type distinction performs real work here: calling this a snare would erase the enormous protective value the rule still delivers to every citizen facing state discrimination; calling it a rope would erase the declared victim set and the enforcement machinery the foreclosure requires. Tangled_rope holds both truths. The R5 mismatch consumer finds status=contested paired with verdict=world_rearranges — a live dispute, not the dead-mandate-plus-dependence signature that flags a zombie.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is the formal_equality_reading of kernel fourteenth_amendment_equal_protection; what structurally changes if the anti_caste_reading displaces it?',
    'Doctrinal evolution: reviewing-court composition and holdings across admissions, contracting, and school-assignment lines, joined to ratification-history scholarship on the Thirty-Ninth Congress''s purpose.',
    'Under the anti-caste reading, state corrective action leaves the victim set and enters the coordinated set; the same referent re-authors at high epsilon; the arrangement''s classification shifts from tangled_rope toward snare-flavored capture of remedial capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this story is one of two rival readings of the Equal Protection kernel.').

omega_variable(
    compelling_justification_valve_viability,
    'Is the compelling-justification exception a live safety valve that bounds the rule''s burden on remedial classification, or a near dead letter?',
    'Track strict-scrutiny survival outcomes: count race-conscious state programs surviving review after Adarand and SFFA versus struck down or abandoned under litigation risk.',
    'If the valve is a dead letter, effective extraction on remedial programs exceeds the authored reading-indexed epsilon and the rule operates as a near-absolute prohibition; if live, the authored bounded-burden picture stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compelling_justification_valve_viability, empirical, 'Whether the exception clause functionally bounds the prohibition.').

omega_variable(
    dual_position_incidence_ambiguity,
    'Do racial minority citizens sit net-beneficiary (shielded from hostile classification) or net-target (stripped of corrective tools) under this rule?',
    'Disaggregate incidence for the same population: compare realized protection value (voting, policing, services, jury-selection cases) against realized foreclosure cost (admissions, contracting, assignment losses).',
    'Net-beneficiary placement supports a rope-leaning computation for the largest affected seat; net-target placement raises effective extraction for that seat and pushes the aggregate toward snare-side readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_position_incidence_ambiguity, empirical, 'Incidence ambiguity for the dual-positioned citizen seat.').

omega_variable(
    background_freeze_scoping_ambiguity,
    'Is treating structural inequality as pre-constitutional background a neutral scoping decision, or is the freeze itself an allocative act this rule performs?',
    'Conceptual analysis joined to outcome data: compare disparity trajectories under formal-equality regimes versus jurisdictions adopting race-conscious correction, and ask whether background status is descriptively stable or maintained by the rule''s own enforcement.',
    'If the freeze is counted as the rule''s own output, the authored epsilon understates extraction materially and this story converges toward the anti-caste sibling''s high-epsilon account of the same referent; if genuinely background, the authored low-moderate epsilon stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(background_freeze_scoping_ambiguity, conceptual, 'The crux separating this reading''s epsilon from the anti-caste sibling''s epsilon over the same referent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__formal_equality_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(faep_formal_eq_tr_t0, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t0, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t10, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 10, 0.09).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t10, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t20, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t20, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t30, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 30, 0.13).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t30, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t40, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t40, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t50, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t50, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t60, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 60, 0.24).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t60, observed).
narrative_ontology:measurement(faep_formal_eq_tr_t70, fourteenth_amendment_equal_protection__formal_equality_reading, theater_ratio, 70, 0.28).
narrative_ontology:measurement_basis(faep_formal_eq_tr_t70, observed).

% Extraction over time
narrative_ontology:measurement(faep_formal_eq_be_t0, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(faep_formal_eq_be_t0, observed).
narrative_ontology:measurement(faep_formal_eq_be_t10, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 10, 0.19).
narrative_ontology:measurement_basis(faep_formal_eq_be_t10, observed).
narrative_ontology:measurement(faep_formal_eq_be_t20, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 20, 0.23).
narrative_ontology:measurement_basis(faep_formal_eq_be_t20, observed).
narrative_ontology:measurement(faep_formal_eq_be_t30, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 30, 0.27).
narrative_ontology:measurement_basis(faep_formal_eq_be_t30, observed).
narrative_ontology:measurement(faep_formal_eq_be_t40, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(faep_formal_eq_be_t40, observed).
narrative_ontology:measurement(faep_formal_eq_be_t50, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 50, 0.34).
narrative_ontology:measurement_basis(faep_formal_eq_be_t50, observed).
narrative_ontology:measurement(faep_formal_eq_be_t60, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 60, 0.36).
narrative_ontology:measurement_basis(faep_formal_eq_be_t60, observed).
narrative_ontology:measurement(faep_formal_eq_be_t70, fourteenth_amendment_equal_protection__formal_equality_reading, base_extractiveness, 70, 0.38).
narrative_ontology:measurement_basis(faep_formal_eq_be_t70, observed).

% Suppression requirement over time
narrative_ontology:measurement(faep_formal_eq_su_t0, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 0, 0.66).
narrative_ontology:measurement_basis(faep_formal_eq_su_t0, observed).
narrative_ontology:measurement(faep_formal_eq_su_t10, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement_basis(faep_formal_eq_su_t10, observed).
narrative_ontology:measurement(faep_formal_eq_su_t20, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(faep_formal_eq_su_t20, observed).
narrative_ontology:measurement(faep_formal_eq_su_t30, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement_basis(faep_formal_eq_su_t30, observed).
narrative_ontology:measurement(faep_formal_eq_su_t40, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement_basis(faep_formal_eq_su_t40, observed).
narrative_ontology:measurement(faep_formal_eq_su_t50, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(faep_formal_eq_su_t50, observed).
narrative_ontology:measurement(faep_formal_eq_su_t60, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(faep_formal_eq_su_t60, observed).
narrative_ontology:measurement(faep_formal_eq_su_t70, fourteenth_amendment_equal_protection__formal_equality_reading, suppression_requirement, 70, 0.4).
narrative_ontology:measurement_basis(faep_formal_eq_su_t70, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__formal_equality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__formal_equality_reading, fourteenth_amendment_equal_protection__anti_caste_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Equal Protection' covers two structurally distinct claims that share one kernel text. This file (formal_equality_reading) and the anti_caste_reading file instantiate different constraints with different epsilon over the SAME referent — state action under the formal-equality doctrine as it operates. This reading authors epsilon 0.38 because it counts hierarchy's persistence as pre-constitutional background; the sibling authors high epsilon because it counts the same persistence as the arrangement's own output. Neither reading is upstream of the other; they are rivals for the same interpretive seat, linked here so contamination and drift analysis can track the contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
