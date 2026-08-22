% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__originalist_reading, []).

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
 *   constraint_id: us_constitution_1787__originalist_reading
 *   human_readable: Constitutional Meaning Fixed at Ratification (Originalist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   The originalist reading of the U.S. Constitution asserts that
 *   constitutional meaning was fixed at ratification (1787 for the original
 *   document, 1791 for the Bill of Rights, and at ratification for each
 *   subsequent amendment) and that the framers' intent or original public
 *   meaning is binding on contemporary interpreters. This reading emerged as
 *   a self-conscious methodology in the 1970s-1980s as a response to
 *   Warren/Burger Court living constitutionalism, but claims continuity with
 *   the founding era's own understandings. The constraint operates through
 *   judicial review: originalist judges invalidate legislative and executive
 *   actions that exceed the fixed meaning, while upholding actions consistent
 *   with it. The coordination function is stabilizing constitutional meaning
 *   against judicial updating; the extraction function is blocking modern
 *   rights claims and regulatory innovations that would be permissible under
 *   living constitutionalism. The constraint has strengthened over time as
 *   originalism captured the federal judiciary (especially the Supreme Court
 *   post-2005).
 *
 * KEY AGENTS:
 *   - originalist_judges: Primary agenda_setters (institutional/arbitrage) — set and enforce the interpretive methodology
 *   - conservative_legal_movement: Beneficiaries (organized/arbitrage) — built the pipeline, funds the infrastructure, reaps policy victories
 *   - marginalized_groups_claiming_new_rights: Primary victims (powerless/trapped) — blocked from constitutional protection for claims outside 1787/1868/1920 understandings
 *   - living_constitutionalist_judges: Victims (powerful/constrained) — their interpretive authority is suppressed where originalism dominates
 *   - congress_legislating_modern_social_policy: Victims (institutional/constrained) — legislative innovations struck down as exceeding fixed meaning
 *   - federalist_society_network: Beneficiaries (organized/arbitrage) — the personnel and intellectual infrastructure of the constraint
 *   - state_governments_seeking_autonomy: Beneficiaries (powerful/mobile) — gain protection from federal regulation under originalist federalism
 *   - legal_academy_historians: Excluded (moderate/trapped) — professional historians often disagree with originalist historical claims but lack institutional voice in judicial decision-making
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_1787__originalist_reading, 0.42).
domain_priors:theater_ratio(us_constitution_1787__originalist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(us_constitution_1787__originalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__originalist_reading, "Constitutional Meaning Fixed at Ratification (Originalist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__originalist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_1787__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__originalist_reading, '715a4be8-fe88-474b-a9d4-87371f4eb5ac').
narrative_ontology:cs_kernel_codification('715a4be8-fe88-474b-a9d4-87371f4eb5ac', fixed_text).
narrative_ontology:cs_authority_grounding('715a4be8-fe88-474b-a9d4-87371f4eb5ac', lineage).
narrative_ontology:cs_interpretation_layer_present('715a4be8-fe88-474b-a9d4-87371f4eb5ac').
narrative_ontology:cs_reading_relation('715a4be8-fe88-474b-a9d4-87371f4eb5ac', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_reading_relation('715a4be8-fe88-474b-a9d4-87371f4eb5ac', us_constitution_1787__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('715a4be8-fe88-474b-a9d4-87371f4eb5ac', foundational, constitutional_meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(constitutional_meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('715a4be8-fe88-474b-a9d4-87371f4eb5ac', constitutional_meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('715a4be8-fe88-474b-a9d4-87371f4eb5ac', foundational, judicial_bound_by_original_public_meaning).
narrative_ontology:cs_axiom_status(judicial_bound_by_original_public_meaning, holdable).
narrative_ontology:cs_axiom_grounding('715a4be8-fe88-474b-a9d4-87371f4eb5ac', judicial_bound_by_original_public_meaning, deontological).
narrative_ontology:cs_axiom('715a4be8-fe88-474b-a9d4-87371f4eb5ac', secondary, historical_methodology_as_constraint).
narrative_ontology:cs_axiom_status(historical_methodology_as_constraint, holdable).
narrative_ontology:cs_axiom_grounding('715a4be8-fe88-474b-a9d4-87371f4eb5ac', historical_methodology_as_constraint, empirically_contingent).
narrative_ontology:cs_reference_frame('715a4be8-fe88-474b-a9d4-87371f4eb5ac', founding_era_understanding).
narrative_ontology:cs_drift_state('715a4be8-fe88-474b-a9d4-87371f4eb5ac', contemporary_originalist_dominance, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('715a4be8-fe88-474b-a9d4-87371f4eb5ac', '2026-08-03T12:00:00Z').
narrative_ontology:cs_kernel_id(us_constitution_1787__originalist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, originalist_judges).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, federalist_society_network).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, state_governments_seeking_autonomy).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, marginalized_groups_claiming_new_rights).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, congress_legislating_modern_social_policy).
narrative_ontology:constraint_victim(us_constitution_1787__originalist_reading, administrative_state_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__originalist_reading, living_constitutionalist_judges).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, popular_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, rule_of_law_as_fixed_meaning).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, judicial_restraint_principle).
narrative_ontology:constraint_vindicates(us_constitution_1787__originalist_reading, enumerated_powers_limitation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the authoritative interpretation of constitutional meaning through judicial review. They select and apply historical methodology (original intent, original public meaning, original methods) to invalidate or uphold government actions. Their decisions bind all lower courts and the political branches. They have arbitrage-grade exit — they could adopt a different methodology but face no professional penalty for originalism; indeed, it is the path to advancement in the current institutional ecology.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, originalist_judges, agenda_setter,
    institutional, biographical, arbitrage, national).

% Built the intellectual, institutional, and personnel infrastructure of originalism over four decades: law school chapters, journals, clerkship networks, judicial vetting. They reap policy victories (gun rights, religious liberty, federalism, deregulation) without bearing the costs of democratic persuasion. They have arbitrage exit — they could pivot to other methodologies but originalism delivers their preferred outcomes.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, conservative_legal_movement, beneficiary,
    organized, generational, arbitrage, national).

% Seek constitutional protection for claims unrecognized in 1787/1868: reproductive autonomy, LGBTQ+ equality, voting rights restoration, criminal justice reform, positive rights to housing/healthcare/education. The originalist constraint blocks these claims by fixing the scope of constitutional protection at a historical baseline that excluded them. Exit is trapped: constitutional amendment requires supermajorities they cannot command; state constitutions offer partial relief but no federal floor; the Supreme Court's originalist majority is life-tenured.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, marginalized_groups_claiming_new_rights, payer,
    powerless, generational, trapped, national).

% Practice a rival methodology that reads constitutional text as establishing principles adaptable to modern conditions. They are marginalized on the current Supreme Court but retain influence in lower courts, academia, and public discourse. They bear the cost of having their interpretive authority structurally suppressed. Constrained exit: they remain judges but their methodology is treated as illegitimate by the dominant coalition; they cannot 'switch' without abandoning their judicial philosophy.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, living_constitutionalist_judges, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__originalist_reading, living_constitutionalist_judges, beneficiary).

% Enacts legislation addressing modern problems (climate, healthcare, voting rights, worker protection, digital privacy) that originalist judges may strike down as exceeding enumerated powers or violating originalist federalism. They bear the cost of legislation being invalidated after democratic enactment. Constrained exit: they can sometimes draft around originalist doctrines (spending power, commerce clause workarounds) but face structural blocking of transformative legislation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, congress_legislating_modern_social_policy, payer,
    institutional, biographical, constrained, national).

% Implement regulatory programs (environmental, financial, workplace safety, consumer protection) that originalist non-delegation and major questions doctrines threaten. They bear the cost of judicial invalidation or constraint of regulatory authority. Constrained exit: they can adjust rulemaking procedures but cannot escape the structural skepticism of originalist judges toward administrative power.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, administrative_state_agencies, payer,
    institutional, biographical, constrained, national).

% The personnel and intellectual infrastructure of the originalist constraint: student chapters, practice groups, judicial vetting, academic conferences, funding pipelines. They benefit directly from the constraint's dominance — their members get clerkships, judgeships, academic positions, and policy influence. Arbitrage exit: they dominate the current ecology but could adapt if the institutional winds shifted.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, federalist_society_network, beneficiary,
    organized, generational, arbitrage, national).

% Use originalist federalism (anti-commandeering, sovereign immunity, enumerated powers limits) to resist federal regulation on issues from healthcare to environmental policy to gun control. They benefit from the constraint's limitation of federal power. Mobile exit: they can pursue policy goals through state legislation and state constitutions regardless of federal doctrine; the constraint amplifies their autonomy but is not their only path.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, state_governments_seeking_autonomy, beneficiary,
    powerful, biographical, mobile, regional).

% Professional historians of the founding era who often find originalist historical claims selective, anachronistic, or methodologically unsound. Their expertise is relevant to the constraint's epistemic demands but they have no institutional voice in judicial decision-making. They are structurally excluded from the authoritative interpretation of the history the constraint claims to honor. Trapped exit: they can publish critiques but cannot affect the constraint's operation.
narrative_ontology:constraint_stakeholder(us_constitution_1787__originalist_reading, legal_academy_historians, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes constitutional meaning against judicial updating by anchoring interpretation in a fixed historical reference point (ratification-era understanding), thereby constraining judicial discretion and providing a determinate rule of recognition for constitutional law.
% TRANSFER_FUNCTION: Moves policy authority from democratic branches (Congress, state legislatures, administrative agencies) and marginalized claimants to a fixed historical meaning enforced by life-tenured judges, transferring the power to define constitutional rights and governmental authority from the living polity to the founding generation.
% ABSENT_VOICES: The founding generation themselves (cannot speak to modern conditions); professional historians (excluded from authoritative interpretation); future generations (bound by a meaning they had no role in creating); marginalized groups who were excluded from the ratification process (women, enslaved people, non-property-owners, indigenous nations). These voices would object to having their constitutional fate determined by a historical moment that denied them participation.
% DISAPPEARANCE_RATIONALE: If the originalist constraint vanished overnight, the Supreme Court would likely revert to living constitutionalist or common-law constitutionalist methodology within one appointment cycle. Modern rights claims (reproductive autonomy, LGBTQ+ equality, voting rights, positive economic rights) would become constitutionally cognizable. The administrative state would regain deference. Federal regulatory power would expand. The entire architecture of originalist federalism (anti-commandeering, major questions, non-delegation) would dissolve. The constitutional order would reorganize around judicial updating rather than historical fixation.
% FOUNDING_PROBLEM: The perceived problem of judicial discretion unbounded by text or history: Warren and Burger Court decisions (e.g., Roe v. Wade, busing orders, expansive criminal procedure rulings) were experienced by the conservative legal movement as judicial legislation — judges imposing policy preferences under the guise of constitutional interpretation. Originalism was built to solve this by fixing meaning at ratification.
% FOUNDING_PROBLEM_CORROBORATION: Originalist proponents (Federalist Society, originalist judges, conservative legal scholars) attest the founding problem remains live — judicial activism continues under living constitutionalist guise. Living constitutionalist scholars (Balkin, Strauss, Tribe) and historians (Wood, Bailyn, Gordon-Reed) attest the founding problem was never purely about discretion but about which values the Constitution protects — and that originalism now replicates the problem in reverse by freezing rights at an exclusionary baseline. The corroboration is split along the same ideological lines that constitute the constraint's beneficiary/victim structure.
narrative_ontology:disappearance_verdict(us_constitution_1787__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__originalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(us_constitution_1787__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__originalist_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_1787__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_1787__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is high because the constraint blocks a wide range of modern governance and rights claims — the administrative state, unenumerated rights, evolving standards of decency — transferring policy authority from democratic branches to a fixed historical meaning. Suppression (0.42) is moderate: the constraint operates through judicial hierarchy and appointment politics rather than direct coercion, but life tenure and the difficulty of constitutional amendment make exit nearly impossible for those subject to it. Theater ratio (0.22) is low-moderate: the historical methodology is genuinely practiced, but methodological indeterminacy (intent vs. public meaning, level of generality) creates space for outcome-driven reasoning. Accessibility collapse (0.78) is high: once the originalist frame is accepted, alternatives (living constitutionalism, common law constitutionalism) appear as judicial activism rather than legitimate interpretation. Resistance (0.55) is substantial: the living constitutionalist tradition, academic critique, and periodic political backlash contest the constraint continuously.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist judge seat (agenda_setter, institutional, arbitrage exit), the constraint is genuine coordination — it solves the problem of judicial discretion by anchoring meaning in history. From the marginalized claimant seat (victim, powerless, trapped), the same structure is extraction — it freezes constitutional protection at a moment when their rights were unrecognized, and the epistemic barrier (historical methodology) blocks their claims. From the living constitutionalist judge seat (victim, powerful, constrained), the constraint is a rival methodology that has captured the institutional high ground. The engine computes these divergences from the structural data: same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist judges and the Federalist Society network are structural beneficiaries (d ~ 0.15): they control the methodology, reap the policy gains, and have arbitrage-grade exit (they could switch methodologies but don't). Marginalized groups claiming new rights are structural victims (d ~ 0.95): they bear the full cost of frozen meaning with no exit (constitutional amendment is practically impossible). Congress and administrative agencies are victims with constrained exit (d ~ 0.80): they can sometimes work around originalist rulings but face structural blocking. State governments seeking autonomy are beneficiaries with mobile exit (d ~ 0.25): they gain from originalist federalism but could pursue policy through state constitutions. Living constitutionalist judges are victims with constrained exit (d ~ 0.70): they remain on courts but their methodology is institutionally marginalized. Legal historians are excluded (d ~ 0.60): their expertise is relevant but structurally barred from authoritative adoption.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (judicial discretion/unbounded interpretation) is contested as live: originalists argue it remains the central danger; living constitutionalists argue the founding problem was static meaning's inability to address modern governance, which originalism now replicates in reverse. The mandate has NOT been resolved — the constraint has strengthened rather than atrophied. Originalism prevents mislabeling coordination as extraction by genuinely constraining its adherents (originalist judges sometimes reach outcomes they dislike), but it also prevents mislabeling extraction as coordination by freezing rights at a historically exclusionary baseline. The classification as tangled_rope captures this duality: real coordination function (stabilizing meaning, constraining judicial will) + asymmetric extraction (blocking modern rights claims, empowering a specific ideological coalition).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the originalist reading a distinct constraint with its own ε, or merely an interpretive lens on the same constitutional constraint?',
    'Compare the structural footprint: if the originalist reading names different beneficiaries/victims, produces different effective extraction patterns across seats, and generates different resistance dynamics than the living or positivist readings, it is a separate constraint instantiation of the same kernel. The ε-invariance principle (DP-001) requires separate stories when structural footprint differs.',
    'If a separate constraint, the originalist reading must author its own stakeholder surface, metrics, and classification. If not, the kernel is the single constraint and readings are observational positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether this reading instantiates a distinct constraint story from its sibling readings of the same kernel.').

omega_variable(
    historical_evidence_accessibility,
    'Can the historical evidence required by originalism (framers'' intent, original public meaning) be reliably recovered, or is the epistemic demand itself a structural barrier that functions as suppression?',
    'Empirical study of originalist opinions: frequency of historical disagreement among originalist judges, divergence between originalist and historian consensus, and whether methodological disputes (intent vs. public meaning vs. methods) produce systematically different outcomes.',
    'If evidence is reliably recoverable, the high epistemic demand is a genuine coordination cost. If evidence is indeterminate and methodological choice drives outcomes, the epistemic barrier functions as suppression favoring those who control the interpretive methodology.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_accessibility, empirical, 'Whether the historical evidence originalism requires is a genuine coordination cost or a structural barrier that extracts from those who cannot meet it.').

omega_variable(
    living_reading_foreclosure,
    'Does the originalist reading''s core premise (meaning fixed at ratification) logically foreclose the living reading''s core premise (meaning evolves with society) within a single legal framework, or do they coexist as competing positions held by different judicial coalitions?',
    'Analyze whether any single court or legal system has ever maintained both as simultaneously valid interpretive methodologies for the same constitutional provisions, or whether adoption of one requires rejection of the other at the level of institutional practice.',
    'If forecloses, the relation is ''forecloses'' and the kernel has a structural fault line. If coexists_with, both readings are live positions in ongoing contest. If influences, originalism''s institutional success creates downstream pressure on living constitutionalism''s legitimacy conditions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_reading_foreclosure, conceptual, 'Structural relationship between originalist and living constitutionalist readings within a single framework.').

omega_variable(
    positivist_reading_boundary,
    'Does the positivist reading (text + democratic amendments only) occupy a distinct structural position from originalism, or does it collapse into originalism when original public meaning aligns with textual meaning?',
    'Compare outcomes: when original public meaning and plain textual meaning diverge (e.g., ''cruel and unusual,'' ''due process''), do positivist and originalist judges reach different conclusions? If they converge, the structural boundary is porous.',
    'If distinct, three separate constraints exist. If positivism collapses into originalism in practice, the kernel has effectively two structural instantiations, not three.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivist_reading_boundary, empirical, 'Whether the positivist reading is structurally distinct from the originalist reading or converges with it in practice.').

omega_variable(
    suppression_mechanism_judicial_appointment,
    'Is the constraint''s suppression structural (Senate confirmation process, life tenure, institutional hierarchy) or internalized (professional socialization, legitimacy beliefs that make non-originalist argument unthinkable for originalist judges)?',
    'Track judicial behavior after appointment: do originalist judges ever depart from originalist methodology when it would produce outcomes they politically favor? Do living constitutionalist judges face professional costs for their methodology?',
    'If internalized, effective suppression is higher than structural measures suggest — the constraint carries its enforcement inside the agents. If structural, suppression is contingent on institutional arrangements that could change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_judicial_appointment, empirical, 'Whether the originalist constraint''s enforcement is external (institutional) or internalized (professional identity).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__originalist_reading, 1787, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t1787, us_constitution_1787__originalist_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t1865, us_constitution_1787__originalist_reading, theater_ratio, 1865, 0.1).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t1937, us_constitution_1787__originalist_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t1973, us_constitution_1787__originalist_reading, theater_ratio, 1973, 0.18).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t2008, us_constitution_1787__originalist_reading, theater_ratio, 2008, 0.2).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_tr_t2026, us_constitution_1787__originalist_reading, theater_ratio, 2026, 0.22).

% Extraction over time
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t1787, us_constitution_1787__originalist_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t1865, us_constitution_1787__originalist_reading, base_extractiveness, 1865, 0.35).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t1937, us_constitution_1787__originalist_reading, base_extractiveness, 1937, 0.48).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t1973, us_constitution_1787__originalist_reading, base_extractiveness, 1973, 0.58).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t2008, us_constitution_1787__originalist_reading, base_extractiveness, 2008, 0.65).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_be_t2026, us_constitution_1787__originalist_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t1787, us_constitution_1787__originalist_reading, suppression_requirement, 1787, 0.2).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t1865, us_constitution_1787__originalist_reading, suppression_requirement, 1865, 0.28).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t1937, us_constitution_1787__originalist_reading, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t1973, us_constitution_1787__originalist_reading, suppression_requirement, 1973, 0.38).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t2008, us_constitution_1787__originalist_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(us_constitution_1787__originalist_reading_su_t2026, us_constitution_1787__originalist_reading, suppression_requirement, 2026, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__originalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__originalist_reading, 0.08).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__living_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, us_constitution_1787__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, unenumerated_rights_doctrine).
narrative_ontology:affects_constraint(us_constitution_1787__originalist_reading, federalism_modern_regulatory_state).

% DUAL FORMULATION NOTE:
% This constraint is one member of the us_constitution_1787 kernel family. The originalist reading (this story) claims meaning fixed at ratification; the living_reading claims meaning evolves with society; the positivist_reading claims meaning is text + democratic amendments only. They differ in ε (originalist: 0.68, living: estimated ~0.55, positivist: estimated ~0.40), beneficiary/victim structure, and coordination function. The originalist reading forecloses the living reading's core premise within a single framework; it coexists with the positivist reading as competing positions; it influences both by capturing the judicial appointment pipeline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, powerful, 0.7).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, powerless, 0.95).
constraint_indexing:directionality_override(us_constitution_1787__originalist_reading, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
