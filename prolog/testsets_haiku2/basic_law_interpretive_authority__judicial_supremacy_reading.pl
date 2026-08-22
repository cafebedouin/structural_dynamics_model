% ============================================================================
% CONSTRAINT STORY: basic_law_interpretive_authority__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_basic_law_interpretive_authority__judicial_supremacy_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: basic_law_interpretive_authority__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional_law/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial supremacy reading of the
 *   contested kernel 'basic law interpretive authority.' The reading holds
 *   that courts possess final, binding interpretive authority over
 *   constitutional meaning, grounded in specialized legal expertise,
 *   independence from electoral cycles, and the discipline of precedent.
 *   Under this reading, legislatures and electoral majorities occupy the
 *   victim set: when courts invalidate legislation as unconstitutional, the
 *   democratic mandate is subordinated to judicial interpretation. The
 *   constraint exhibits tangled-rope structure: genuine coordination function
 *   (preventing constitutional crisis, settling meaning disputes through
 *   reasoned interpretation) paired with asymmetric extraction (judiciary
 *   retains agenda-setting power, elected branches bear gridlock costs). The
 *   judicial supremacy reading coexists with parliamentary sovereignty and
 *   popular constitutionalism readings of the same kernel—each reading is a
 *   different constraint with different victim/beneficiary structures and
 *   different ε values.
 *
 * KEY AGENTS:
 *   - judiciary_institutional_authority: Beneficiary and agenda-setter—exercises final interpretive power, sets binding precedent, vetos legislation deemed unconstitutional.
 *   - elected_legislature: Payer and secondary beneficiary—bears gridlock costs when courts block legislation, benefits from certain constitutional protections (constrained exit).
 *   - electoral_majorities: Payer—elects representatives expecting policy enactment; mandate frustrated when courts invalidate legislation (constrained exit via amendment).
 *   - appointed_judges: Beneficiary—hold lifetime tenure, prestige, and disproportionate institutional influence; their identity fused with the supremacy doctrine.
 *   - rights_claimants: Beneficiary—protected by court enforcement of rights against majoritarian legislation (mobile exit in principle, dependent on judicial protection in practice).
 *   - constitutional_originalists: Excluded—contest the reading and seek to constrain judicial authority to original meaning; their objection does not alter the institutional structure.
 *   - constitutional_academics: Observer—analyze and theorize the structure but do not set or enforce the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(basic_law_interpretive_authority__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(basic_law_interpretive_authority__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(basic_law_interpretive_authority__judicial_supremacy_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(basic_law_interpretive_authority__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(basic_law_interpretive_authority__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(basic_law_interpretive_authority__judicial_supremacy_reading, "constitutional_law/institutional_design").

domain_priors:requires_active_enforcement(basic_law_interpretive_authority__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(basic_law_interpretive_authority__judicial_supremacy_reading, '3221b2fd-e14b-4e42-af0c-bdf8f16e19ed').
narrative_ontology:cs_kernel_codification('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', formalized).
narrative_ontology:cs_authority_grounding('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', lineage).
narrative_ontology:cs_interpretation_layer_present('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed').
narrative_ontology:cs_reading_relation('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', basic_law_interpretive_authority__parliamentary_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', basic_law_interpretive_authority__popular_constitutionalism_reading, influences).
narrative_ontology:cs_axiom('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', foundational, judicial_expertise_institutional_necessity).
narrative_ontology:cs_axiom_status(judicial_expertise_institutional_necessity, holdable).
narrative_ontology:cs_axiom_grounding('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', judicial_expertise_institutional_necessity, empirically_contingent).
narrative_ontology:cs_axiom('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', foundational, judicial_independence_from_electoral_pressure).
narrative_ontology:cs_axiom_status(judicial_independence_from_electoral_pressure, holdable).
narrative_ontology:cs_axiom_grounding('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', judicial_independence_from_electoral_pressure, deontological).
narrative_ontology:cs_reference_frame('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', marbury_framework_judicial_review).
narrative_ontology:cs_drift_state('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', contemporary_counter_majoritarian_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('3221b2fd-e14b-4e42-af0c-bdf8f16e19ed', '').
narrative_ontology:cs_kernel_id(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institutional_authority).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_victim(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, appointed_judges).
narrative_ontology:constraint_beneficiary(basic_law_interpretive_authority__judicial_supremacy_reading, rights_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts claim and exercise final authority to interpret constitutional text and doctrine, binding legislatures and executives to their readings. They justify this authority through specialized legal training, insulation from electoral cycles, and the reasoning tradition of judicial review. This institutional position yields direct power: courts can void legislative acts, invalidate executive orders, and set binding precedent that shapes the constitutional landscape for generations.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institutional_authority, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures must operate within judicial-imposed constitutional boundaries set by courts' interpretations. They pay through gridlock: legislation vetted as constitutional by the democratic process is invalidated by courts, forcing re-drafting or abandonment. They also benefit from courts' enforcement of certain constitutional protections that constrain executive overreach. Their exit is constrained: they cannot simply ignore adverse court rulings without triggering constitutional crisis.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(basic_law_interpretive_authority__judicial_supremacy_reading, elected_legislature, beneficiary).

% Voters elect legislators expecting them to enact policy reflecting electoral mandate. When courts block legislation as unconstitutional, that mandate is frustrated—the policy voters approved through elections is declared invalid by an unelected body. They pay through the inability to translate electoral victories into lasting policy when courts disagree with legislative constitutional readings. They cannot directly exit; their recourse is lengthy constitutional amendment.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, electoral_majorities, payer,
    organized, biographical, constrained, national).

% Individual judges benefit from the institutional supremacy doctrine: they hold lifetime tenure, considerable prestige, and influence disproportionate to their democratic accountability. The judicial supremacy reading locates their authority in expertise and independence; their personal exit is constrained by institutional identity and professional norms.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, appointed_judges, beneficiary,
    institutional, generational, trapped, national).

% Individuals and groups asserting constitutional rights benefit from courts having authority to enforce those rights against majoritarian legislation. When courts strike down laws as rights-violating, minority protections persist regardless of electoral pressure. They have moderate exit in principle (relocate, organize politically for constitutional amendment) but depend on judicial protection to remain present in their jurisdiction.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, rights_claimants, beneficiary,
    moderate, biographical, mobile, national).

% Scholars, judges, and politicians who hold that the Constitution's meaning is fixed at ratification object to court-driven doctrinal evolution and argue for limiting judicial authority to the text's original public meaning. They are excluded from determining the constraint's operation: the institutional supremacy structure persists regardless of whether courts adhere to originalism. Their objection goes unheard in terms that shape the enforced rule.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_originalists, excluded,
    moderate, biographical, constrained, national).

% Legal scholars analyze and critique the structure of judicial authority, offering rival interpretive theories and empirical analyses of doctrinal drift. They occupy an analytical seat: they describe and debate but do not directly set or enforce the rules.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, constitutional_academics, observer,
    moderate, generational, analytical, national).

% The constitutional amendment process is the formal escape hatch from judicial interpretations deemed unacceptable. It is rarely used and requires supermajority consensus, making it structurally difficult to exercise. As a mechanism, it is consistently subordinated to judicial supremacy: the court's reading remains in force unless and until an amendment overcomes it, giving the judiciary a supremacy by default until affirmatively contradicted.
narrative_ontology:constraint_stakeholder(basic_law_interpretive_authority__judicial_supremacy_reading, amendment_process, excluded,
    analytical, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(basic_law_interpretive_authority__judicial_supremacy_reading, amendment_process).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(basic_law_interpretive_authority__judicial_supremacy_reading, judiciary_institutional_authority).
narrative_ontology:fixing_cost_class(basic_law_interpretive_authority__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, reasoned mechanism to resolve disputes about constitutional meaning that would otherwise trigger recurring constitutional crises. A single interpretive authority bound by precedent and reasoning tradition enables settled law and predictable governance, rather than constitutional meaning oscillating with every electoral cycle.
% TRANSFER_FUNCTION: Moves final interpretive power from electoral majorities and legislatures to the judiciary. Elected branches can propose constitutional readings through legislation; courts adjudicate finality. The transfer includes the ability to veto majoritarian policy deemed unconstitutional and to set binding precedent that constrains future legislative action.
% ABSENT_VOICES: Popular sovereigntists and parliamentary supremacy advocates are structurally excluded from determining how the constraint operates. They contest the constraint's very legitimacy—arguing that constitutional meaning should emerge from democratic contestation or legislative authority—but their objection does not alter the institutional fact of judicial supremacy. Amendment advocates who wish to override specific court decisions cannot do so without supermajority consensus, effectively silencing them unless they can mobilize that supermajority.
% DISAPPEARANCE_RATIONALE: If judicial supremacy in constitutional interpretation vanished overnight, governance would reorganize around rival authority structures. Either legislatures would treat their own constitutional readings as binding (shifting to parliamentary supremacy), or constitutional meaning would emerge from ongoing democratic contestation without terminal adjudication. The entire structure of constitutional law—precedent, judicial review, separation of powers doctrine—depends on courts holding final authority. Its removal would dissolve the institutional settlement.
% FOUNDING_PROBLEM: Early constitutional governance was plagued by legislative overreach, executive arbitrariness, and the absence of a neutral arbiter for constitutional disputes. Marbury v. Madison (1803) and subsequent doctrine established courts as the authoritative interpreters, solving the crisis of constitutional meaning oscillating with political factions and enabling settled, reasoned constitutional law.
% FOUNDING_PROBLEM_CORROBORATION: Judiciary and most constitutional scholars defend the reading: courts remain essential to protecting rights against majoritarian legislation and preventing constitutional instability. Parliamentary sovereignty and popular constitutionalism advocates, however, dispute that the founding problem persists—they argue that settled constitutional meaning can emerge from legislatures and ongoing democratic contestation. Empirical evidence from legislative abdication on constitutional questions (the rise of 'political questions' doctrine) supports the judicial reading; evidence of judicial overreach and counter-majoritarian difficulty supports the alternative readings. No consensus from outside the benefiting parties.
narrative_ontology:disappearance_verdict(basic_law_interpretive_authority__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(basic_law_interpretive_authority__judicial_supremacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(basic_law_interpretive_authority__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(basic_law_interpretive_authority__judicial_supremacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(basic_law_interpretive_authority__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(basic_law_interpretive_authority__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.48 (moderate, reflecting genuine coordination function) and rises to 0.68 by period 25, plateauing thereafter. This trajectory models increasing recognition of the constraint as extractive: early constitutional governance benefited from judicial resolution of authority disputes (coordination dominates); over time, courts' doctrinal expansion and counter-majoritarian difficulty became salient (extraction accumulates). Theater ratio rises from 0.25 to 0.42, indicating growing performative maintenance: courts increasingly justify decisions through formalist legal reasoning while pragmatic policy considerations drive outcomes. Suppression requirement rises from 0.58 to 0.71, reflecting increasing need for active enforcement to maintain the supremacy doctrine against scholarly critique, legislative pushback (Court-packing rhetoric, jurisdiction-stripping proposals), and the lived cost of counter-majoritarian outcomes. The plateau at period 25 indicates a steady-state constraint: extraction and suppression requirements stabilize once they reach the level needed to sustain the arrangement against standing resistance. Accessibility collapse at 0.62 reflects moderate foreclosure of alternatives: while amendment and legislative defiance remain formally possible, the transaction costs and supermajority requirements make them rarely successfully executed. Resistance at 0.73 is substantial: elected branches, scholars, and electoral movements consistently push against judicial supremacy, proposing limits on judicial review, originalist constraint, and legislative coequality. The combination of high extraction and high suppression identifies a tangled rope with substantial asymmetry: the coordination benefit (settled constitutional meaning) is real but does not justify the entire extraction load.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's institutional seat, this constraint is genuinely rope: courts prevent constitutional chaos by providing final authority, and the independence and expertise justification is integral to the arrangement's legitimacy. From the legislature's seat, the same structure operates as snare: the court-imposed constitutional reading forecloses legislation the legislature deems constitutionally valid, suppresses legislative constitutional judgment, and transfers interpretive authority irreversibly (legislative defiance triggers constitutional crisis). From electoral-majority seats, it is pure snare: the democratic mandate is subordinated to an unelected branch's interpretation. The engine computes this perspective divergence from the structural data (beneficiary/victim declarations, power atoms, exit options); the story-level claim (tangled rope) represents the judicial supremacy reading's own characterization of itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Judiciary: d ≈ 0.0–0.1 (full beneficiary seat). Exercises final authority, collects the constraint's primary benefit (institutional supremacy), faces no real exit pressure. Power is institutional and temporal—lifetime tenure insulates from electoral pressure. Exit is analytical: judges cannot practically exit the institutional role without triggering crisis; they are bound by professional identity. Elected legislature: d ≈ 0.7–0.8 (target seat). Bears gridlock costs when courts invalidate legislation. Power is powerful but constrained by the constitutional supremacy doctrine—they cannot simply ignore adverse rulings without constitutional crisis. Exit is constrained: they must re-draft legislation to satisfy judicial interpretation or seek amendment (supermajority requirement). Secondary beneficiary role reflects that they also benefit from courts constraining executive overreach. Electoral majorities: d ≈ 0.75–0.85 (target seat, constrained exit). Their policy mandate is frustrated by judicial interpretation they did not endorse and cannot directly override. Exit through amendment requires supermajority consensus—effectively trapped. Rights claimants: d ≈ 0.2–0.3 (moderate beneficiary). Benefit from court enforcement of rights; depend on judicial supremacy for protection against majorities. Exit is mobile in principle (relocate, political organization) but depends on judicial protection to remain present. Appointed judges: d ≈ 0.1 (beneficiary, identity-locked). Individual judges benefit from institutional supremacy; exit is identity-locked—professional identity fused with the institutional role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing constitutional crisis through stable authority) is contested in status: judiciary and mainstream constitutional scholars hold it live; parliamentary sovereignty and popular constitutionalism advocates hold it dead or substantially solved. The disappearance verdict is world_rearranges: governance depends on judicial supremacy to coordinate authority. The mismatch between contested status and world_rearranges does not trigger immediate mandatrophy declaration, because the constraint is still actively defended by institutional beneficiaries and intellectual tradition. However, the high resistance measurement (0.73) and the growth in theater ratio (from 0.25 to 0.42) suggest performance maintenance pressure: the constraint is increasingly defended through formalism and institutional position rather than by the coordinate coordination function. If the founding problem drifts from contested to dead (empirically, if legislatures and majorities develop stable alternative authority mechanisms), mandatrophy would activate. The analytical prediction: the constraint shows early-stage decay dynamics consistent with piton transition if the founding problem's death status becomes undeniable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expertise_grounding_empirical_test,
    'Do courts'' specialized legal expertise and reasoning discipline actually produce constitutionally superior or more stable outcomes than legislatures exercising coequal interpretive authority?',
    'Comparative institutional analysis comparing constitutional outcomes (stability, rights protection, democratic responsiveness) across jurisdictions with judicial supremacy vs. parliamentary coequality (e.g., UK post-2005 Human Rights Act, Canada, Australia). Empirical studies of doctrinal drift and counter-majoritarian difficulty.',
    'If courts'' expertise produces superior outcomes, the coordination function claim is strengthened and extraction is justified as coordination cost. If no superior outcome is evident, the extraction is revealed as pure institutional rent-seeking, and the constraint reclassifies to snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_grounding_empirical_test, empirical, 'Whether judicial expertise justifies institutional supremacy or whether it is post-hoc legitimation of institutional power.').

omega_variable(
    independence_requirement_mechanism,
    'Is lifetime tenure and political insulation necessary for the coordination function, or does it enable extraction that could be maintained with different accountability structures?',
    'Historical and comparative analysis of judicial independence mechanisms: do term limits, election-based selection, or other accountability structures produce less stable constitutional interpretation, or do they primarily shift power distribution without affecting stability? Case evidence from jurisdictions that have altered judicial selection and tenure.',
    'If tenure is structurally necessary for stability, it is coordination cost. If alternative mechanisms maintain stability while reducing judicial supremacy, tenure is extractive architecture protecting institutional power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(independence_requirement_mechanism, empirical, 'Whether lifetime tenure is essential to constitutional coordination or a contingent institutional choice protecting judicial power.').

omega_variable(
    kernel_reading_determination,
    'Is the judicial supremacy reading the correct reading of the basic_law_interpretive_authority kernel, or is one of the sibling readings (parliamentary sovereignty or popular constitutionalism) more defensible?',
    'This is structurally irreducible: different reading traditions, different constitutional sources (text, practice history, democratic theory) support each reading. The question routes to political preference and jurisprudential commitment rather than empirical resolution.',
    'The reading chosen determines which constraint is instantiated: judicial supremacy creates beneficiaries in the judiciary and victims in elected branches; parliamentary sovereignty swaps those roles; popular constitutionalism dissolves terminal authority altogether. The ε values differ sharply across readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_determination, preference, 'Which reading of the basic law kernel is constitutionally valid—a question of jurisprudential commitment and political theory rather than empirical fact.').

omega_variable(
    suppression_mechanism_structural_internalized,
    'Is the suppression measured (0.71) structural (legal barriers, institutional architecture, precedent walls) or partly internalized (lawyers and judges have absorbed the normative supremacy claim as self-evident)?',
    'Post-exit trajectory: if a legislature explicitly rejected judicial supremacy (e.g., through legislation asserting coequal interpretive authority), would suppression persist or would the constraint evaporate? Does the suppression persist in scholarly and professional communities even when institutional power is absent?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests—judges and lawyers self-enforce the doctrine. If purely structural, removing institutional backing could dissolve the constraint''s hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_internalized, empirical, 'Whether measured suppression is structural enforcement or internalized professional commitment.').

omega_variable(
    counter_majoritarian_difficulty_asymmetry,
    'Does judicial supremacy genuinely protect minority rights against majoritarian overreach, or does it primarily protect previously-established distributions against democratic reclamation?',
    'Empirical analysis of actual outcomes: do courts more often block majoritarian legislation that infringes rights of vulnerable minorities, or do they more often preserve institutional advantages, property rights, and existing class distributions? Comparison of rights-protective vs. distribution-protective invalidations.',
    'If courts primarily protect vulnerable minorities, the arrangement''s extraction is justified by coordination/protection function and the tangled-rope classification is stable. If courts primarily preserve existing distributions, the extraction is revealed as protecting entrenched interests, and the constraint reclassifies toward snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(counter_majoritarian_difficulty_asymmetry, empirical, 'Whether judicial review primarily constrains majoritarian rights violation or protects existing distributions against democratic change.').

omega_variable(
    amendment_process_accessibility,
    'Is the constitutional amendment process genuinely available as an escape from judicial interpretation, or is the supermajority requirement so stringent that it functions as permanent subordination to judicial reading?',
    'Empirical analysis of amendment success rates when constituencies explicitly wish to override a judicial interpretation. Comparison of jurisdictions with different amendment requirements. Analysis of whether amendment failure is due to lack of consensus on the constitutional question or merely to supermajority engineering.',
    'If amendment is practically available, victim exit is constrained but not trapped—extractiveness is lower. If amendment is perpetually blocked by veto players, victim exit is trapped and extractiveness approaches pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_process_accessibility, empirical, 'Whether amendment process provides functional escape route or merely illusory redress.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(basic_law_interpretive_authority__judicial_supremacy_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(basi_tr_t0, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(basi_tr_t0, observed).
narrative_ontology:measurement(basi_tr_t5, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement_basis(basi_tr_t5, observed).
narrative_ontology:measurement(basi_tr_t10, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement_basis(basi_tr_t10, observed).
narrative_ontology:measurement(basi_tr_t15, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(basi_tr_t15, observed).
narrative_ontology:measurement(basi_tr_t20, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(basi_tr_t20, observed).
narrative_ontology:measurement(basi_tr_t25, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(basi_tr_t25, observed).
narrative_ontology:measurement(basi_tr_t30, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(basi_tr_t30, observed).
narrative_ontology:measurement(basi_tr_t35, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(basi_tr_t35, observed).
narrative_ontology:measurement(basi_tr_t40, basic_law_interpretive_authority__judicial_supremacy_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(basi_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(basi_be_t0, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(basi_be_t0, observed).
narrative_ontology:measurement(basi_be_t5, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(basi_be_t5, observed).
narrative_ontology:measurement(basi_be_t10, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement_basis(basi_be_t10, observed).
narrative_ontology:measurement(basi_be_t15, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 15, 0.61).
narrative_ontology:measurement_basis(basi_be_t15, observed).
narrative_ontology:measurement(basi_be_t20, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement_basis(basi_be_t20, observed).
narrative_ontology:measurement(basi_be_t25, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(basi_be_t25, observed).
narrative_ontology:measurement(basi_be_t30, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(basi_be_t30, observed).
narrative_ontology:measurement(basi_be_t35, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(basi_be_t35, observed).
narrative_ontology:measurement(basi_be_t40, basic_law_interpretive_authority__judicial_supremacy_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(basi_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(basi_su_t0, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(basi_su_t0, observed).
narrative_ontology:measurement(basi_su_t5, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(basi_su_t5, observed).
narrative_ontology:measurement(basi_su_t10, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(basi_su_t10, observed).
narrative_ontology:measurement(basi_su_t15, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(basi_su_t15, observed).
narrative_ontology:measurement(basi_su_t20, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(basi_su_t20, observed).
narrative_ontology:measurement(basi_su_t25, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(basi_su_t25, observed).
narrative_ontology:measurement(basi_su_t30, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(basi_su_t30, observed).
narrative_ontology:measurement(basi_su_t35, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(basi_su_t35, observed).
narrative_ontology:measurement(basi_su_t40, basic_law_interpretive_authority__judicial_supremacy_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(basi_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(basic_law_interpretive_authority__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(basic_law_interpretive_authority__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__parliamentary_sovereignty_reading).
narrative_ontology:affects_constraint(basic_law_interpretive_authority__judicial_supremacy_reading, basic_law_interpretive_authority__popular_constitutionalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel basic_law_interpretive_authority. The kernel itself—how final interpretive constitutional authority is held—admits multiple structurally distinct readings. The judicial supremacy reading locates authority in courts, grounded in expertise and independence; it yields high judicial beneficiary extraction and legislative/majoritarian victim positions. The parliamentary sovereignty reading locates authority in elected legislatures, reversing those positions. The popular constitutionalism reading dissolves terminal authority altogether, yielding a different ε and constraint type entirely. These are not the same constraint measured from different angles—they are genuinely distinct constraints with different beneficiary/victim structures, different ε values, and different classification implications. Each story instantiates one reading cleanly. They are linked through network.affects_constraints because the supremacy of one reading forecloses or influences the others: if judicial supremacy is fully entrenched, parliamentary sovereignty reading's institutional basis erodes; if popular constitutionalism gains traction, both institutional supremacy readings lose salience.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
