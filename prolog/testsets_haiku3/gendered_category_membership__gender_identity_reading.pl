% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Gender Category Membership via Identity Self-Declaration (Identity Reading)
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'gendered_category_membership'—the gender-identity reading. Under this
 *   reading, legitimate membership in gendered categories ('woman,' 'man,'
 *   'mother,' 'women's spaces') is grounded in an individual's
 *   self-identified gender rather than reproductive anatomy determined at
 *   birth. Institutions (bathrooms, shelters, prisons, sports divisions,
 *   legal records, quota seats, survivor services) recode their
 *   sex-segregation boundaries to gender-segregation. The reading is not
 *   authored here as a moral claim; it is authored as a constraint structure:
 *   who benefits, who bears cost, what enforcement is required, and what
 *   resistance persists. The measurement series traces the constraint's
 *   extractiveness and suppression rising from early adoption to
 *   institutional entrenchment and then plateauing. Theater rises sharply in
 *   early phases (policy is framed as trans inclusion while enforcement
 *   targets sex-category boundaries) then stabilizes as the frame becomes
 *   normalized.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiaries, identity-locked exit, organized power
 *   - cis_women: dual-positioned payers and beneficiaries, lose gatekeeping, retain access
 *   - female_athletes: payers bearing physiological disadvantage asymmetry under gender-segregation recoding
 *   - gender_identity_advocates: beneficiaries setting institutional norms, mobile exit, organized power
 *   - institutional_policy_setters: agenda-setters balancing legal exposure and social pressure, arbitrage exit
 *   - biological_sex_advocates: excluded from norm-setting, trapped power, treated as outside-bounds
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.58).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.62).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Gender Category Membership via Identity Self-Declaration (Identity Reading)").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '9719cdc2-4430-4885-8bac-cce0d47d5a6b').
narrative_ontology:cs_kernel_codification('9719cdc2-4430-4885-8bac-cce0d47d5a6b', distributed).
narrative_ontology:cs_authority_grounding('9719cdc2-4430-4885-8bac-cce0d47d5a6b', extraction).
narrative_ontology:cs_interpretation_layer_present('9719cdc2-4430-4885-8bac-cce0d47d5a6b').
narrative_ontology:cs_reading_relation('9719cdc2-4430-4885-8bac-cce0d47d5a6b', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('9719cdc2-4430-4885-8bac-cce0d47d5a6b', gendered_category_membership__social_role_reading, influences).
narrative_ontology:cs_axiom('9719cdc2-4430-4885-8bac-cce0d47d5a6b', foundational, gender_identity_intrinsic_and_self_determinative).
narrative_ontology:cs_axiom_status(gender_identity_intrinsic_and_self_determinative, holdable).
narrative_ontology:cs_axiom_grounding('9719cdc2-4430-4885-8bac-cce0d47d5a6b', gender_identity_intrinsic_and_self_determinative, deontological).
narrative_ontology:cs_axiom('9719cdc2-4430-4885-8bac-cce0d47d5a6b', foundational, self_identification_legitimate_category_basis).
narrative_ontology:cs_axiom_status(self_identification_legitimate_category_basis, holdable).
narrative_ontology:cs_axiom_grounding('9719cdc2-4430-4885-8bac-cce0d47d5a6b', self_identification_legitimate_category_basis, deontological).
narrative_ontology:cs_reference_frame('9719cdc2-4430-4885-8bac-cce0d47d5a6b', sex_based_institutional_categorization).
narrative_ontology:cs_drift_state('9719cdc2-4430-4885-8bac-cce0d47d5a6b', contemporary_post_identity_movement_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9719cdc2-4430-4885-8bac-cce0d47d5a6b', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, female_athletes_in_sex_segregated_sports).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, cis_women_athletes_in_integrated_competition).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_excluding_trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, cis_women_athletes_in_integrated_competition).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sex_segregated_domestic_violence_services).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain formal recognition in spaces, records, and categories coded as 'woman'—bathrooms, shelters, sports divisions, prisons, quotas, scholarship categories. The constraint recodes sex-segregated institutions as gender-segregated. Inclusion depends on active enforcement against exclusionary policies; their structural position requires continuous institutional reengineering. Exit from the identity frame is identity-locked by definition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_women, beneficiary,
    organized, biographical, identity_locked, national).

% Bear the cost of shared intimate spaces (bathrooms, shelters, locker rooms) now recoded on gender identity rather than sex. Also pay where category-specific resources (scholarships, athletic divisions, domestic-violence funding) are redistributed to include trans women. They retain access to these spaces and categories, but the membership boundary has shifted—they lose gatekeeping authority over who counts as 'woman' and must accommodate entrants they did not previously control. Their resistance to the new boundary is often cast as exclusionary rather than protective.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, cis_women, beneficiary).

% Compete in divisions nominally sex-segregated (coded for athletic advantage mitigation via reproductive biology) now reframed as gender-segregated (open to anyone with gender-identity claim). The constraint recodes the segregation boundary; athletes bear physiological advantage asymmetry if trans women with male-pattern development compete in women's divisions. Their objections are framed as bigotry rather than physical-performance equity concerns, creating suppression via reputational cost.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, female_athletes_in_sex_segregated_sports, payer,
    moderate, biographical, constrained, national).

% Gain institutional validation of the gender-identity framework as legitimate category basis. They set norms in education, healthcare, legal systems, and workplace policy. They benefit from constraint entrenchment because each institutional reengineering (pronouns, bathrooms, quotas, record sex markers) normalizes the framework and makes reversal incrementally harder. They face moderate exit costs—leaving the movement is possible but requires ideological realignment.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, gender_identity_advocates, beneficiary,
    organized, biographical, mobile, national).

% A subset of cis women who advocate sex-based category boundaries and resist gender-identity recoding. Under the constraint, they are positioned as the perpetrators of exclusion—their insistence on sex-based categories is reframed as exclusionary rather than boundary-protective. The constraint's enforcement machinery treats their resistance as discrimination. Their position is identity-locked because backing down means accepting a definition of womanhood that they believe erases sex-based analysis and material reality.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_excluding_trans_women, payer,
    moderate, biographical, identity_locked, national).

% Administer recoding of institutional categories and spaces from sex-based to gender-identity-based. They set bathroom policy, update legal sex markers, create pronoun requirements, redraw athletic divisions, allocate quota seats. They face legal exposure (civil-rights suits from excluded parties) and social pressure from both trans-inclusion advocates and exclusionary-boundary defenders. They have the most exit optionality—switching policy costs reputational capital but is technically possible for any single institution.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, institutional_policy_setters, agenda_setter,
    institutional, generational, arbitrage, national).

% In institutions where women's divisions are redefined to include trans women but retain segregation from men's divisions, these athletes gain the benefit of structural sex-segregation (protected competitive space) while incurring the cost of boundary-redefinition (new competitors with different physiology). They benefit from the category existing and being recognized; they pay through altered competitive conditions and loss of gatekeeping over membership.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, cis_women_athletes_in_integrated_competition, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, cis_women_athletes_in_integrated_competition, payer).

% Shelters and advocacy organizations structured around sex-segregated space for abuse survivors. The constraint requires them to either accept gender-identity self-declaration (allowing entry of anyone who claims female gender identity) or face legal/reputational consequences for discrimination. They pay through resource strain, policy conflict, survivor-care complexity, and potential loss of funding or legal standing. Their mission (survivor safety) creates genuine tension with the constraint's boundary-redefinition, but the constraint's enforcement treats resistance as bigotry.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sex_segregated_domestic_violence_services, payer,
    moderate, biographical, constrained, local).

% Defend sex-based category boundaries and argue for biological reproduction as the legitimate basis for category membership. They are excluded from the institutional decision-making process—policy is set without their participation, and their arguments are treated as outside-the-bounds (not worthy of serious engagement). They experience institutional suppression via institutional closure and reputational cost for speaking.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, biological_sex_advocates, excluded,
    organized, biographical, trapped, national).

% Asymmetrically positioned by this constraint. They claim male gender identity but may retain female reproductive anatomy. The constraint recodes women's categories to admit trans women but does not symmetrically recode men's categories (social/legal pressure against trans men in men's spaces remains high). They are excluded from this particular constraint's benefit structure even though the gender-identity principle should apply to them equally. The constraint is read as male-centric in operation even if framed as gender-neutral in principle.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, trans_men, excluded,
    moderate, biographical, identity_locked, national).

% Analytical seat documenting the constraint's structure without endorsing either the gender-identity or sex-based reading. Maps the institutional enforcement, identifies the asymmetric positions, and measures the constraint's extractiveness from each seat.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, observer_neutral_on_identity, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, gender_identity_advocates).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns institutional categories and spaces with self-identified gender identity rather than biological sex: solves the coordination problem of how to classify people for purposes of eligibility, belonging, and institutional access when identity claims diverge from biological markers. Enables individuals whose gender identity does not match reproductive anatomy to be recognized in their affirmed category rather than forced-assigned category.
% TRANSFER_FUNCTION: Transfers gatekeeping authority over category membership from those whose reproductive anatomy determines canonical inclusion (under sex-based reading) to individuals' self-declaration. Moves legitimacy: from biological markers to subjective identity as the category-membership basis. Moves administrative burden: from individuals seeking affirmation (who had to prove transition) to institutions (who now adjudicate identity claims).
% ABSENT_VOICES: Biological sex advocates and trans-exclusionary feminists are excluded from institutional norm-setting; they represent live objections that never enter the decision-making room. Sex-segregated survivor advocates (domestic violence, sexual assault services) are present but asymmetrically positioned—their expertise in survivor safety is treated as less legitimate than trans-inclusion advocates' claims. Trans men are absent from the benefits structure even though the principle applies to them equally.
% DISAPPEARANCE_RATIONALE: If the gender-identity reading vanished and institutions reverted to sex-based category membership, trans women would lose formal recognition in institutional spaces (bathrooms, shelters, prisons, sports, quotas), cis women would regain gatekeeping authority over category membership, sex-segregated spaces would revert to sex-based allocation, and the institutional-recognition infrastructure supporting gender transition (legal sex-marker changes, pronoun policies) would collapse. Institutions would reorganize around biological sex as the legitimate category basis.
% FOUNDING_PROBLEM: Medical and psychological understanding evolved to recognize gender dysphoria and gender identity as distinct from reproductive anatomy; trans and non-binary individuals experienced institutional misrecognition and exclusion. The founding problem was lack of institutional recognition for identity-based gender, forcing misgendering, legal misidentification, and exclusion from affinity spaces. The constraint was built to solve institutional non-recognition.
% FOUNDING_PROBLEM_CORROBORATION: Trans and gender-identity advocates attest the founding problem is live: institutional non-recognition causes persistent psychological harm and material exclusion. Biological sex advocates and sex-segregated service providers attest the problem is overstated: institutional sex categorization has legitimate functions (athletic fairness, safety in intimate spaces) not solved by identity-declaration alone. Medical professionals in gender dysphoria care attest the founding problem (lack of recognition) was real; controversy centers on what level of institutional recoding solves it versus what creates new problems.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.58 at interval end) because the constraint redistributes gatekeeping authority (cis women lose control over membership definition) and moves institutional resources (toward trans inclusion, away from sex-segregated service models), but does not involve direct monetary transfer. Suppression is high (0.62) because the constraint requires active institutional enforcement—without it, existing sex-segregation boundaries persist; the enforcement machinery also suppresses dissenting arguments by treating them as bigotry rather than legitimate boundary-defense positions. Theater rises sharply (0.22 to 0.41 over first 12 time points) because the constraint is initially framed as simple trans inclusion while enforcement actually targets sex-segregation recoding; as policy becomes normalized, theater plateaus (the frame is accepted, enforcement is less novelty-dramatic). The measurement series shows extractiveness rising and plateauing (institutional adoption complete by t=20), suppression rising and plateauing (enforcement infrastructure built by t=20), and theater rising and plateauing (novelty period ends). The plateau indicates the constraint has reached institutional equilibrium, not that extraction has ceased.
 *
 * PERSPECTIVAL GAP:
 *   From the trans_women seat: the constraint solves a real problem (institutional non-recognition) and redistributes harm fairly (cis women lose authority, gain inclusion). From the cis_women seat: the constraint imposes costs (loss of gatekeeping, shared intimate space with people they did not vet) and reframes their resistance as perpetrator-status rather than boundary-protection. From the institutional_policy_setters seat: the constraint creates legal liability risk (either direction—exclude trans women, face civil-rights suits; include trans women, face sex-discrimination suits). The engine computes these divergent positions per seat from the structural data; the gap is the measurement the system is built to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans_women_d: near 0.0 (full beneficiary)—they benefit from gatekeeping-authority redistribution without running the constraint themselves (role: beneficiary, not agenda_setter). Their identity_locked exit amplifies: they cannot leave the gender-identity frame without ceasing to be trans-identified. Cis_women_d: near 0.5 (symmetric)—they retain access to women's spaces (benefit) but lose gatekeeping and incur resource costs (cost). Female_athletes_d: near 0.8 (target)—they bear physiological asymmetry costs with constrained exit (cannot avoid competing in gender-segregated divisions). Gender_identity_advocates_d: near 0.2 (beneficiary)—they collect institutional power without institutional cost, with mobile exit. Institutional_policy_setters_d: near 0.5 (symmetric)—they gain legitimacy from trans inclusion, bear legal exposure from both directions. Biological_sex_advocates_d: near 1.0 (full target)—they bear reputational cost for speaking, have no institutional power, trapped in the excluded position.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is Tangled Rope, not Snare: it possesses a genuine coordination function (institutional recognition of gender identity) AND asymmetric extraction (gatekeeping-authority redistribution that benefits trans women and disadvantages cis women). Active enforcement distinguishes it from Rope (which would coordinate without requiring coercion). The constraint is not a pure lie (Snare), because the coordination function is real—institutions DO benefit from having a unified policy rather than inconsistent sex-vs-gender-identity application. However, the coordination function is embedded in an extractive frame: the redistribution is not framed as a tradeoff (you gain institutional consistency, you lose gatekeeping control) but as a moral imperative (inclusion, with resistance reframed as bigotry). This framing asymmetry—where coordination is highlighted and extraction is obscured—is what makes the theater component significant. A Rope would say 'we are redesigning categories for consistency, here are the costs and benefits'; Tangled Rope says 'we are including trans people, and those who object are perpetrators.' The constraint could shift toward Snare if enforcement intensity increases (stronger suppression of dissenting voices, more reputational targeting, institutional capture of biological-sex advocacy spaces). It could shift toward Rope if the coordination and extraction components are openly acknowledged and tradeoffs are negotiated—but that shift would require breaking the current framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression primarily structural (institutional barriers, policy exclusion, legal restrictions) or internalized (narrative reframing of resistance as bigotry, identity-fusion pressure on both trans and cis women)?',
    'Post-policy-change tracking: if suppression persists after institutional barriers are removed (e.g., trans women gain bathroom access but cis women continue reporting identity-fusion pressure to accept the frame), the suppression is partially internalized. If suppression collapses when institutional barriers fall, it is primarily structural.',
    'If suppression is primarily internalized, the constraint''s effective power is higher than the structural measure suggests—the targets carry the suppression with them even after institutional change. This would argue for higher classification confidence in Snare or Piton. If primarily structural, the extraction is institutional rather than psychological, suggesting institutional reversal is possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Decomposing suppression mechanism into structural and internalized components.').

omega_variable(
    coordination_function_necessity,
    'Is the gender-identity-based recoding of categories structurally necessary to solve the coordination problem of recognizing gender-diverse individuals, or is there an alternative coordination solution that addresses recognition without recoding sex-segregation boundaries?',
    'Comparative institutional analysis: jurisdictions that create gender-identity-specific categories (gender-neutral bathrooms, third category in legal sex markers, gender-identity-separate sports divisions) without recoding existing sex-segregation provide data on alternative coordination structures.',
    'If alternative coordination structures can satisfy trans recognition without recoding sex-segregation boundaries, the sex-recoding component is extractive rather than necessary for coordination—reclassifying closer to Snare. If sex-recoding is necessary for coordination (because trans inclusion in women''s spaces is itself the coordination goal, not merely a byproduct), the coordination function is genuine and Tangled Rope is the stable classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_necessity, conceptual, 'Whether sex-segregation recoding is necessary to coordination or merely chosen as the implementation.').

omega_variable(
    biological_sex_reading_foreclosure,
    'Does the gender-identity reading logically foreclose the biological-sex reading within a single coherent framework, or do the two readings coexist as distinct positions held by different institutional actors?',
    'Formal logical analysis of the foundational premises: if gender-identity is defined as intrinsic and immutable AND sex is defined as reproductive anatomy, do these premises contradict (foreclosure) or describe different categorization axes (coexistence)? Empirical check: do institutions adopt exclusively one reading, or do they layer both (gender-identity categories alongside sex-based segregation for specific functions)?',
    'If readings foreclose each other: the constraint is structurally unstable; one must eliminate the other. If readings coexist: the constraint operates in an institutional landscape where both readings remain live, creating persistent contestation and boundary-policing. Foreclosure would argue for terminal classification; coexistence argues for stable Tangled Rope classification with ongoing institutional conflict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_reading_foreclosure, conceptual, 'Whether gender-identity and biological-sex readings logically contradict or structurally coexist.').

omega_variable(
    institutional_capture_by_advocates,
    'To what extent does the constraint''s persistence depend on institutional capture by gender-identity advocates, versus genuine coordination necessity and broader social consensus?',
    'Survey institutional decision-making: how many institutions that adopted gender-identity-based recoding did so through democratic/representative process versus administrative decree or leadership capture? What proportion of cis women actively consents to the recoding versus tolerate it? Where resistance is strongest (sex-segregated services, athletic competition), is it minoritarian or majority within those sectors?',
    'High capture would suggest the constraint is Snare (minority extracts via institutional power, suppressing majority opposition). Low capture would support Tangled Rope (multiple parties coordinate, asymmetric extraction is acknowledged). Majority consent despite cost would argue stable Tangled Rope. Minority consent under suppression would argue Snare evolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_by_advocates, empirical, 'Whether the constraint reflects institutional capture or broader consensus.').

omega_variable(
    trans_men_asymmetry,
    'Why does the gender-identity reading asymmetrically benefit trans women but not trans men (who claim male identity but face less institutional recoding of men''s sex-segregation)?',
    'Institutional audit: compare recoding intensity for women''s vs. men''s categories. Track policy on bathrooms, shelters, prisons, sports divisions separately by gender. Measure institutional momentum (which reading is more normalized in which institutional contexts).',
    'If asymmetry is real and substantial: it suggests the constraint''s operation is not truly gender-neutral (as the reading claims) but male-to-female-inclusive, hinting at capture or political power differential. It would argue the reading-level axioms may not be holding symmetrically, creating a foreclosure or instability risk.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(trans_men_asymmetry, empirical, 'Asymmetric benefits to trans women versus trans men suggest non-neutral reading implementation.').

omega_variable(
    kernel_reading_family_structure,
    'How do the three sibling readings (biological_sex, gender_identity, social_role) relate structurally? Do they foreclose each other pairwise, or do some coexist while others foreclose?',
    'Formal analysis of core premises: biological_sex reading grounds in reproductive anatomy (empirically contingent, measurable). Gender_identity reading grounds in subjective identity (deontological, immeasurable). Social_role reading grounds in observable performance and recognition (empirical and deontological mixed). Determine logical contradictions and institutional coexistence patterns.',
    'If readings form a total ordering (one forecloses the next, which forecloses the next), the family structure is hierarchical and unstable. If readings form a Hasse diagram (some pairs coexist, other pairs foreclose), the family structure is more complex and stable. This affects how institutional evolution proceeds—linear foreclosure vs. fragmented coexistence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_family_structure, conceptual, 'Structural relations among the three readings of gendered_category_membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__gender_identity_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gend_tr_t4, gendered_category_membership__gender_identity_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(gend_tr_t8, gendered_category_membership__gender_identity_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__gender_identity_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement(gend_tr_t16, gendered_category_membership__gender_identity_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__gender_identity_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__gender_identity_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__gender_identity_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(gend_be_t4, gendered_category_membership__gender_identity_reading, base_extractiveness, 4, 0.45).
narrative_ontology:measurement(gend_be_t8, gendered_category_membership__gender_identity_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__gender_identity_reading, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(gend_be_t16, gendered_category_membership__gender_identity_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__gender_identity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__gender_identity_reading, base_extractiveness, 25, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__gender_identity_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(gend_su_t4, gendered_category_membership__gender_identity_reading, suppression_requirement, 4, 0.51).
narrative_ontology:measurement(gend_su_t8, gendered_category_membership__gender_identity_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__gender_identity_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(gend_su_t16, gendered_category_membership__gender_identity_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__gender_identity_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__gender_identity_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__gender_identity_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gender_affirming_medical_access).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, pronoun_institutional_policy).

% DUAL FORMULATION NOTE:
% This constraint is part of the gendered_category_membership kernel family (three readings decomposed from a single contested kernel per ε-invariance principle). All three readings share the kernel but instantiate different constraints with different beneficiaries, ε values, and claims. Gender_identity_reading (this story) claims Tangled Rope with moderate ε (0.58); biological_sex_reading would claim Mountain with low ε; social_role_reading would claim different extraction structure. The three stories are linked via network.affects_constraints (all sibling pairs); the engine maps institutional adoption of each reading across sectors and time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__gender_identity_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
