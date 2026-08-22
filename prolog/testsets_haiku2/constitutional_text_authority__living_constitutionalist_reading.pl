% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__living_constitutionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: constitutional_text_authority__living_constitutionalist_reading
 *   human_readable: Constitutional Text Authority — Living Constitutionalist Reading
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   The living constitutionalist reading of the Constitution holds that
 *   constitutional meaning evolves with social attitudes and values, and that
 *   judicial authority derives from contemporary moral principles applied to
 *   ancient text. This is ONE READING of the contested kernel
 *   'constitutional_text_authority' — a standing commitment (the written
 *   Constitution) that different judicial coalitions and legal scholars read
 *   through radically different interpretive lenses. The living
 *   constitutionalist reading (instantiated here) holds that the judiciary
 *   legitimately recognizes unenumerated rights and adapts constitutional
 *   meaning to contemporary understanding. The originalist reading holds that
 *   meaning is fixed at ratification. The positivist reading maintains a
 *   law/morality distinction and grounds validity in formal enactment
 *   procedures. Each reading instantiates a different constraint: different ε
 *   (extractiveness), different beneficiaries and victims, different
 *   authority structures. This JSON documents ONLY the living
 *   constitutionalist reading, as a clean ε-invariant constraint. Sibling
 *   readings are SEPARATE constraint stories (not authored here) linked via
 *   network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__living_constitutionalist_reading, 0.68).
domain_priors:suppression_score(constitutional_text_authority__living_constitutionalist_reading, 0.52).
domain_priors:theater_ratio(constitutional_text_authority__living_constitutionalist_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(constitutional_text_authority__living_constitutionalist_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__living_constitutionalist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__living_constitutionalist_reading, "Constitutional Text Authority — Living Constitutionalist Reading").
narrative_ontology:topic_domain(constitutional_text_authority__living_constitutionalist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__living_constitutionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__living_constitutionalist_reading, 'f92a6737-a8ef-4462-b58b-01a1ffddc752').
narrative_ontology:cs_kernel_codification('f92a6737-a8ef-4462-b58b-01a1ffddc752', fixed_text).
narrative_ontology:cs_authority_grounding('f92a6737-a8ef-4462-b58b-01a1ffddc752', extraction).
narrative_ontology:cs_interpretation_layer_present('f92a6737-a8ef-4462-b58b-01a1ffddc752').
narrative_ontology:cs_reading_relation('f92a6737-a8ef-4462-b58b-01a1ffddc752', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f92a6737-a8ef-4462-b58b-01a1ffddc752', constitutional_text_authority__positivist_reading, influences).
narrative_ontology:cs_axiom('f92a6737-a8ef-4462-b58b-01a1ffddc752', foundational, constitutional_meaning_evolves_with_values).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves_with_values, holdable).
narrative_ontology:cs_axiom_grounding('f92a6737-a8ef-4462-b58b-01a1ffddc752', constitutional_meaning_evolves_with_values, deontological).
narrative_ontology:cs_axiom('f92a6737-a8ef-4462-b58b-01a1ffddc752', foundational, contemporary_moral_principle_source_of_authority).
narrative_ontology:cs_axiom_status(contemporary_moral_principle_source_of_authority, holdable).
narrative_ontology:cs_axiom_grounding('f92a6737-a8ef-4462-b58b-01a1ffddc752', contemporary_moral_principle_source_of_authority, deontological).
narrative_ontology:cs_axiom('f92a6737-a8ef-4462-b58b-01a1ffddc752', secondary, unenumerated_rights_recognizable_through_evolution).
narrative_ontology:cs_axiom_status(unenumerated_rights_recognizable_through_evolution, holdable).
narrative_ontology:cs_axiom_grounding('f92a6737-a8ef-4462-b58b-01a1ffddc752', unenumerated_rights_recognizable_through_evolution, deontological).
narrative_ontology:cs_reference_frame('f92a6737-a8ef-4462-b58b-01a1ffddc752', adaptive_constitutional_legitimacy).
narrative_ontology:cs_drift_state('f92a6737-a8ef-4462-b58b-01a1ffddc752', contemporary_originalist_counter_movement, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f92a6737-a8ef-4462-b58b-01a1ffddc752', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__living_constitutionalist_reading, contemporary_rights_advocates).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, textual_originalists).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, federalism_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, state_legislatures).
narrative_ontology:constraint_victim(constitutional_text_authority__living_constitutionalist_reading, congress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution according to living constitutionalist methodology: recognizes that constitutional meaning evolves with social attitudes and values, that contemporary moral principles inform interpretation, and that unenumerated rights can be identified through evolving understanding. This role grants the judiciary authority to declare constitutional requirements without formal amendment (Brown v. Board paradigm). The judiciary both sets the agenda (decides which contemporary values count) and benefits (expands interpretive authority, legitimizes landmark decisions).
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary, beneficiary).

% Advocate for recognition of rights not explicitly textually enumerated — privacy rights, gender equality, LGBTQ+ protections, expanded free speech. The living constitutionalist reading legitimizes their claims by making 'contemporary moral principles' a source of constitutional authority. They depend on judicial adoption of this reading to achieve constitutional protection for their agenda. Their influence on constitutional doctrine is amplified when the judiciary embraces contemporary values as interpretive source.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, contemporary_rights_advocates, beneficiary,
    organized, biographical, constrained, national).

% Argue that constitutional meaning is fixed at the time of ratification by the public meaning of the text, and that judicial imposition of contemporary values violates the rule of law and usurps democratic amendment authority. They are systematically disadvantaged by the living constitutionalist reading, which treats their interpretive methodology as less attentive to moral justice. Originalist judges must operate in an institutional context where living constitutionalism is the dominant methodology, constraining their ability to anchor decisions in original meaning. They bear the cost of interpretive marginalization.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, textual_originalists, payer,
    organized, biographical, constrained, national).

% Argue that the Constitution reserves powers to the states and that the federal judiciary should interpret it narrowly to protect state sovereignty. The living constitutionalist reading expands federal judicial power over state regulation by finding unenumerated protections that override state law. Federalism advocates bear the structural cost of diminished state authority when the judiciary declares that contemporary constitutional values require federal override of state policy.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, federalism_advocates, payer,
    moderate, biographical, constrained, national).

% Enact statutes in areas they believe fall within state constitutional authority. Under the living constitutionalist reading, these statutes face invalidation when the federal judiciary determines that contemporary understanding of constitutional principles (e.g., equality, liberty, privacy) requires different outcomes. State legislatures bear the cost of reduced autonomy and the practical burden of litigation defending policies that the judiciary may invalidate as out-of-step with contemporary values.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, state_legislatures, payer,
    powerful, biographical, constrained, national).

% Holds formal amendment authority under Article V but finds that authority functionally diminished when the judiciary recognizes constitutional protections without amendment. Congress can enact legislation but faces the prospect that federal courts will invalidate it as constitutionally incompatible with contemporary principles. The living constitutionalist reading transfers practical constitutional authority from Article V supermajority amendment to judicial interpretation.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, congress, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text_authority__living_constitutionalist_reading, congress, observer).

% Produce scholarship arguing that the Constitution has fixed meaning and that contemporary judicial adaptation violates the rule of law. They are excluded from the living constitutionalist interpretive framework because their fundamental claim (meaning is fixed, not evolved) contradicts the reading's core premise. While their scholarship is published and heard, it is systematically downweighted in elite law schools and appellate courts that adopt living constitutionalist methodology. Their voice is not banned but structurally marginalized.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, originalist_scholars, excluded,
    moderate, biographical, constrained, national).

% Examine constitutional law from a law/morality distinction perspective, asking whether legal meaning derives from formal sources (text, history, institutional procedure) or moral principle. They observe that the living constitutionalist reading challenges the law/morality distinction by making contemporary moral principles a source of constitutional validity. They occupy an analytical seat, neither collecting from the reading nor bearing its costs, but analyzing its structural implications.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__living_constitutionalist_reading, legal_positivists, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__living_constitutionalist_reading, federal_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__living_constitutionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of updating a centuries-old constitutional text to address modern circumstances without requiring supermajoritarian consensus for every new social challenge. Coordinates judicial authority with evolving democratic values so that constitutional meaning can adapt without the procedural paralysis of Article V amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from textually-constrained methodologies and from supermajoritarian democratic amendment processes toward the federal judiciary's contemporary-values jurisprudence. Moves the locus of constitutional decision-making from the text's historical meaning and formal procedures toward judicial perception of contemporary moral principles. Contemporary rights advocates gain recognition; originalists and federalism advocates lose authority.
% ABSENT_VOICES: Originalist judges and scholars, strict federalism advocates, and state legislative representatives would strongly object to the living constitutionalist reading as usurpation of democratic amendment authority, but their objections are heard within a framework that treats their concerns as less attentive to justice. They are not excluded from the conversation (they argue in law journals and courtrooms) but are structurally positioned as obstinate obstacles to moral progress rather than as legitimate constitutional interpreters.
% DISAPPEARANCE_RATIONALE: If the living constitutionalist reading disappeared overnight and the judiciary reverted to originalist or textualist constraint, constitutional law would be thrown into radical realignment. Brown v. Board, privacy rights doctrine, equal protection under the law as applied to new classes, and unenumerated rights protections would lose their constitutional grounding. Democratic politics would reorganize as states gained authority to regulate domains previously constitutionalized through evolved interpretation. The federal judiciary's role would narrow to historical textual constraint, and the amendment process would become the exclusive path to new constitutional protections.
% FOUNDING_PROBLEM: A constitutional text written in the 18th century cannot on its face address 21st-century circumstances — digital privacy, gender equality, evolving definitions of liberty and personhood. Rigid textual interpretation leaves modern injustices (segregation, discrimination, privacy invasions, asymmetric state power) without constitutional remedy unless and until supermajoritarian amendment occurs. This creates a rule-of-law gap: injustices persist because the text does not explicitly forbid them, even though contemporary moral understanding demands protection.
% FOUNDING_PROBLEM_CORROBORATION: Living constitutionalist judges and contemporary rights advocates attest the founding problem is live and acute — rigidly fixed meaning leaves modern injustices unremediable. Originalist scholars counter that the founding problem is a false crisis: the Constitution's general principles (liberty, equality, due process) handle modern circumstances without judges reading in new rights; and supermajoritarian amendment is the proper path for fundamentally new protections, not judicial reinterpretation. Legal historians are divided: some document Framer intent to leave principles flexible and adaptable; others argue the Framers expected amendment for new circumstances. No corroborating external authority (Congress, state legislatures, or originalist scholars) endorses the founding problem as stated by living constitutionalists; the problem is affirmed internally within the reading's own framework and by contemporary rights advocates who benefit from its solution.
narrative_ontology:disappearance_verdict(constitutional_text_authority__living_constitutionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__living_constitutionalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__living_constitutionalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__living_constitutionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__living_constitutionalist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.68 because the living constitutionalist reading permits judges to declare what contemporary moral principles require of the Constitution — a substantial transfer of interpretive authority from textual constraint and democratic amendment toward judicial discretion. The transfer is real (benefits the judiciary and contemporary rights advocates, costs textual originalists and federalism-constrained states), but the reading legitimizes it as coordination (adapting the Constitution to modern justice demands) rather than raw extraction. Suppression is moderate (0.52) because originalist alternatives exist and are intellectually coherent; the reading does not foreclose them, but institutional dominance of living constitutionalism in elite law schools and federal courts creates structural pressure against originalist voices. Theater ratio (0.41) reflects that while the reading does authentic work (Brown v. Board genuinely changed constitutional outcomes), a growing share of 'constitutional evolution' decisions are justified post-hoc through contemporary values language that could also be framed as pure policy preference. Accessibility collapse is low (0.38) because the originalist alternative remains accessible — it requires judges to do different hermeneutic work, not to accept a natural law. Resistance is high (0.74) because originalists, federalism advocates, and state legislatures mount sustained intellectual and institutional resistance. The measurement series spans 1954 (Brown, landmark living constitutionalist moment) to 2024, showing extractiveness rising from 1954–2000 as the reading became institutional orthodoxy, then plateauing (2000–2024) as originalist counter-movement built in-court presence. Theater ratio rises modestly, suggesting increasing performativity of 'contemporary values' justifications. Suppression requirement rises slightly early (as originalism needed institutional suppression to lose ground) then stabilizes as originalism became entrenched as a coherent minority position.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (originalists, federalism advocates, Congress, state legislatures) perceive the living constitutionalist reading as usurpation of democratic amendment authority and the rule of law — unjustified extraction of power to the unelected judiciary. The beneficiary seats (federal judiciary, contemporary rights advocates) perceive it as necessary coordination — a solution to the problem of adapting a centuries-old text to modern justice demands without supermajoritarian political stagnation. The agenda-setter (federal judiciary) experiences this as legitimate authority delegation from the constitutional text itself; payers experience it as judicial overreach. The engine's per-seat computation should detect this as the reading operating differently for each seat: Mountain-like legitimacy to the beneficiary (natural evolution of constitutional meaning), Tangled-Rope-like from the payer seats (coordination story covering extraction of interpretive authority).
 *
 * DIRECTIONALITY LOGIC:
 *   The federal judiciary benefits directly from the living constitutionalist reading — it expands their interpretive authority and legitimizes landmark decisions without amendment. Their directionality is low (d ≈ 0.2, near beneficiary end). Contemporary rights advocates benefit through recognized unenumerated rights and adaptive remedies; their d ≈ 0.25. Textual originalists and state legislatures are the targets — they bear the cost of having their interpretive framework delegitimized and finding constitutional authority shifted toward the federal judiciary. Their d ≈ 0.8. Congress sits asymmetrically: it retains formal amendment authority but finds it functionally diminished, so its d ≈ 0.55 (partially targeted by reduced practical authority). The divergence between institutional/powerful payers (Congress, state legislatures, organized originalists) and the institutional agenda-setter (federal judiciary) is the core asymmetry the engine should detect. Originalist judges in the federal judiciary experience a different directionality than living constitutionalist judges: the reading extracts from them (undermines their preferred interpretive method) even though they sit at the institutional power level. This within-seat divergence is noted in commentary but captured in the structural data through the victims array (textual_originalists as a distinct agent from federal_judiciary as institutional beneficiary).
 *
 * MANDATROPHY ANALYSIS:
 *   The living constitutionalist reading faces a mandatrophy risk: the founding problem it solves (how to apply a static text to changed circumstances) may be obsolete if political culture evolves toward more-frequent formal amendment or consensus-based interpretation. However, the reading is not dead as a mandate — contemporary moral principle remains a source of judicial legitimacy, especially for questions the Constitution's text does not clearly address. The mandate is contested (originalists argue it was never valid; living constitutionalists argue it is essential), which places it in 'mandatrophy contested' rather than 'mandatrophy resolved.' The Theater ratio rise suggests growing detachment between the stated coordination function (adaptation) and the actual mechanism (judges declaring contemporary values), indicating potential erosion of the mandate's legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_discovery_ambiguity,
    'Does the living constitutionalist reading DISCOVER pre-existing constitutional principles that were always there, or INVENT constitutional meanings in response to contemporary values?',
    'Historical analysis of Framer intent and original public meaning: if the Framers deliberately wrote general principles expecting interpretation to evolve, living constitutionalism is closer to discovery; if the Framers expected amendment for new circumstances, it is closer to invention. Originalist scholarship argues the latter; living constitutionalist scholarship argues the former.',
    'If living constitutionalism is discovery, the reading is a Rope (coordination problem: how to access latent constitutional meaning). If it is invention, the reading is a Snare (extraction: judges imposing new meaning under the guise of discovery). This omega is located in whether the text''s flexibility is inherent or imposed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_discovery_ambiguity, conceptual, 'Ambiguity about whether the reading discovers or invents constitutional meaning.').

omega_variable(
    contemporary_values_boundary,
    'What counts as ''contemporary values''? Is the standard the actual attitudes of a majority of Americans, the moral principles endorsed by legal elites, or the judge''s own reasoned judgment about justice?',
    'Examination of how judges justify decisions appealing to ''contemporary values'' — do they cite polling data, academic consensus, moral philosophy, or unstated judicial intuition? Comparison of cases where contemporary values rhetoric yields divergent outcomes depending on which definition is applied.',
    'If contemporary values is majoritarian, the reading coordinates democratic input with constitutional interpretation (more legitimate). If it is elite consensus or judicial intuition, the reading is closer to judicial override of democratic will (more extractive, higher ε). This ambiguity is located in the authorization source for ''contemporary values.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_values_boundary, empirical, 'Ambiguity about what ''contemporary values'' means and how it is authorized.').

omega_variable(
    originalist_foreclosure_ambiguity,
    'Do the reading''s core premises logically FORECLOSE the originalist reading, or merely challenge its institutional dominance?',
    'Logical analysis: can a judge coherently hold that constitutional meaning has both fixed textual meaning (originalism) AND evolved contemporary meaning (living constitutionalism) in the same framework? If yes, they coexist; if no, one forecloses the other. Current jurisprudence suggests coexistence (originalists and living constitutionalists both sit on courts), suggesting no foreclosure.',
    'If foreclosure occurs, the reading''s relationship to originalism should be reclassified from coexists_with to forecloses in cs_structure.reading_relations. This affects the network topology of the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(originalist_foreclosure_ambiguity, conceptual, 'Whether living constitutionalism logically requires rejecting originalism or merely dominates it institutionally.').

omega_variable(
    institutional_suppression_mechanism,
    'Is the suppression of originalist voices structural (external barriers to publishing, hiring, citation) or internalized (originalists self-censor or accept marginalization)?',
    'Post-marginalization trajectory: if originalists removed from institutional pressure gain voice and influence (e.g., Justice Scalia''s originalism gaining strength in elite law schools post-2000), suppression was internalized and reversible; if they remain marginalized despite reduced pressure, suppression is more structural. Current data shows originalism strengthening in some courts and law schools, suggesting partially internalized suppression.',
    'If internalized, the measured suppression (0.52) understates the constraint''s effective suppression because originalists carry the internalized frame with them after exit. The constraint''s extractiveness might be lower if internalized suppression is subtracted. If structural, the measured suppression is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_mechanism, empirical, 'Whether suppression of originalism is structural or internalized.').

omega_variable(
    kernel_reading_vs_two_constraints,
    'Is this best understood as one kernel (the written Constitution) with multiple readings (living constitutionalism, originalism, positivism), or as two distinct constraints with overlapping scope?',
    'If the readings make genuinely different claims about THE SAME constitutional question (e.g., what does ''liberty'' mean in the Due Process Clause?), they are readings of one kernel. If they address different questions (e.g., living constitutionalism addresses adaptability; originalism addresses textual fidelity), they are separate constraints.',
    'This JSON assumes one kernel with multiple readings and populates cs_structure accordingly. If the resolution shows separate constraints, the network topology and cs_structure fields should be reconsidered, and additional constraint stories authored for the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_two_constraints, conceptual, 'Meta-question: whether the living constitutionalist reading is one reading of a kernel or a structurally independent constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__living_constitutionalist_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t1954, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1954, 0.25).
narrative_ontology:measurement(cons_tr_t1965, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1965, 0.3).
narrative_ontology:measurement(cons_tr_t1980, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 1980, 0.35).
narrative_ontology:measurement(cons_tr_t2000, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(cons_tr_t2015, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2015, 0.42).
narrative_ontology:measurement(cons_tr_t2024, constitutional_text_authority__living_constitutionalist_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(cons_be_t1954, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1954, 0.52).
narrative_ontology:measurement(cons_be_t1965, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1965, 0.58).
narrative_ontology:measurement(cons_be_t1980, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 1980, 0.64).
narrative_ontology:measurement(cons_be_t2000, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2000, 0.68).
narrative_ontology:measurement(cons_be_t2015, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2015, 0.67).
narrative_ontology:measurement(cons_be_t2024, constitutional_text_authority__living_constitutionalist_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t1954, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1954, 0.35).
narrative_ontology:measurement(cons_su_t1965, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1965, 0.4).
narrative_ontology:measurement(cons_su_t1980, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 1980, 0.46).
narrative_ontology:measurement(cons_su_t2000, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2000, 0.52).
narrative_ontology:measurement(cons_su_t2015, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2015, 0.53).
narrative_ontology:measurement(cons_su_t2024, constitutional_text_authority__living_constitutionalist_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__living_constitutionalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__living_constitutionalist_reading, 0.22).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__living_constitutionalist_reading, constitutional_text_authority__positivist_reading).

% DUAL FORMULATION NOTE:
% The constitutional_text_authority kernel has three reading-instantiated constraints: living_constitutionalist_reading (this file), originalist_reading, and positivist_reading. Each reading is a separate constraint with its own ε, beneficiaries, victims, and authority structure. They are linked via network.affects_constraints to show the kernel family structure. The living constitutionalist reading influences the originalist and positivist readings by establishing contemporary values as a source of constitutional legitimacy, which pressures the other readings to either incorporate or explicitly reject that source. This story documents ONLY the living constitutionalist reading; the sibling readings are authored separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_text_authority__living_constitutionalist_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
