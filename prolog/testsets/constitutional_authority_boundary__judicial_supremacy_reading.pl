% ============================================================================
% CONSTRAINT STORY: constitutional_authority_boundary__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_authority_boundary__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_authority_boundary__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy in Constitutional Interpretation
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The judicial supremacy reading asserts that the constitutional text
 *   establishes courts—specifically, the supreme court—as the final,
 *   unchallengeable arbiter of what the constitution means. Under this
 *   reading, when the legislature or executive acts, they must have
 *   constitutional authority; the courts determine whether that authority
 *   exists; and the courts' determination is binding and cannot be overridden
 *   by political process or subsequent legislation (except through formal
 *   constitutional amendment, which itself requires that courts validate the
 *   amendment as consistent with core constitutional principles). This
 *   reading is one of three deeply contested interpretations of the same
 *   constitutional kernel: coordinate construction (three branches interpret
 *   within their spheres), parliamentary primacy (elected legislatures hold
 *   ultimate authority), and judicial supremacy (courts do). These are not
 *   different factual claims about how courts operate; they are different
 *   readings of what the foundational constitutional text requires. Each
 *   reading instantiates a distinct constraint with a distinct ε,
 *   beneficiary/victim structure, and enforcement mechanism. The supremacy
 *   reading produces high extractiveness (0.68) because it concentrates
 *   interpretive monopoly rents in the judiciary and blocks legislative
 *   reversal through ordinary means.
 *
 * KEY AGENTS:
 *   - supreme_court_institution: The institutional beneficiary. Holds and defends interpretive monopoly; the supremacy reading is the reading that justifies this monopoly. Its structural position depends on the supremacy reading being correct.
 *   - legislature: Institutional payer. Subject to post-hoc judicial veto on constitutional grounds; cannot override or revise the Court's reading of its own powers through legislation; requires supermajority consensus for constitutional amendment to reverse Court doctrine.
 *   - executive_branch: Institutional payer. Implements Court orders; subject to judicial review; cannot contest the Court's interpretation of executive authority.
 *   - popular_sovereignty_advocates: Organized payer. Structurally barred from amending the constitution if the Court has ruled the amendment itself unconstitutional; the mechanism for democratic constitutional revision is constrained by the Court's interpretation of what amendments are permissible.
 *   - constitutional_scholars: Analytical observers. Identify the supremacy reading as one interpretive choice among defensible alternatives; document that other democracies and historical American practice show coordinate construction and parliamentary primacy are live readings.
 *   - lower_courts: Beneficiaries of hierarchical clarity and stability; also locked into the supremacy structure—they have no independent interpretive authority and must accept Supreme Court precedent.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, 0.68).
domain_priors:suppression_score(constitutional_authority_boundary__judicial_supremacy_reading, 0.71).
domain_priors:theater_ratio(constitutional_authority_boundary__judicial_supremacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(constitutional_authority_boundary__judicial_supremacy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_authority_boundary__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_authority_boundary__judicial_supremacy_reading, "Judicial Supremacy in Constitutional Interpretation").
narrative_ontology:topic_domain(constitutional_authority_boundary__judicial_supremacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(constitutional_authority_boundary__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_authority_boundary__judicial_supremacy_reading, '79a394bb-155b-43ca-8557-c0d323ceb67d').
narrative_ontology:cs_kernel_codification('79a394bb-155b-43ca-8557-c0d323ceb67d', fixed_text).
narrative_ontology:cs_authority_grounding('79a394bb-155b-43ca-8557-c0d323ceb67d', lineage).
narrative_ontology:cs_interpretation_layer_present('79a394bb-155b-43ca-8557-c0d323ceb67d').
narrative_ontology:cs_reading_relation('79a394bb-155b-43ca-8557-c0d323ceb67d', constitutional_authority_boundary__coordinate_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('79a394bb-155b-43ca-8557-c0d323ceb67d', constitutional_authority_boundary__parliamentary_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('79a394bb-155b-43ca-8557-c0d323ceb67d', foundational, judicial_finality_required).
narrative_ontology:cs_axiom_status(judicial_finality_required, holdable).
narrative_ontology:cs_axiom_grounding('79a394bb-155b-43ca-8557-c0d323ceb67d', judicial_finality_required, deontological).
narrative_ontology:cs_axiom('79a394bb-155b-43ca-8557-c0d323ceb67d', foundational, coordinate_branches_impossible).
narrative_ontology:cs_axiom_status(coordinate_branches_impossible, holdable).
narrative_ontology:cs_axiom_grounding('79a394bb-155b-43ca-8557-c0d323ceb67d', coordinate_branches_impossible, empirically_contingent).
narrative_ontology:cs_reference_frame('79a394bb-155b-43ca-8557-c0d323ceb67d', supreme_court_as_final_arbiter).
narrative_ontology:cs_drift_state('79a394bb-155b-43ca-8557-c0d323ceb67d', contemporary_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('79a394bb-155b-43ca-8557-c0d323ceb67d', '').
narrative_ontology:cs_kernel_id(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, judicial_institution).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_interpreters).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, popular_sovereignty_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, legislature).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, lower_courts).
narrative_ontology:constraint_beneficiary(constitutional_authority_boundary__judicial_supremacy_reading, litigants).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, popular_sovereignty_advocates).
narrative_ontology:constraint_victim(constitutional_authority_boundary__judicial_supremacy_reading, litigants).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, rule_of_law_requires_judicial_finality).
narrative_ontology:constraint_vindicates(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_text_has_stable_meaning).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enforces its claim to final interpretive authority by denying that other branches have co-equal standing. The Court continuously stakes its legitimacy on being the supreme interpreter—any concession that Congress could interpret the Constitution within its sphere would undermine the supremacy reading itself. The Court cannot exit this position without ceasing to be a court under the supremacy framework.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court_institution, agenda_setter,
    institutional, civilizational, trapped, national).

% Drafts legislation, but faces the risk of judicial invalidation if the Court deems the law unconstitutional. Cannot revise the Court's reading of the Constitution through ordinary legislation. Can attempt constitutional amendment to override Court doctrine, but this requires supermajority consensus and the Court may itself judge proposed amendments unconstitutional. Also benefits from the clarity that a final interpreter provides—once the Court has spoken on a constitutional question, legislators know the boundary they must not cross.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, legislature, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, legislature, beneficiary).

% Executes the law as written by the legislature, but must implement Court orders interpreting the Constitution. Cannot challenge the Court's reading of executive powers; doing so would constitute institutional defiance. Must remove judges through impeachment (requiring legislative supermajority), making removal prohibitively costly.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, executive_branch, payer,
    institutional, generational, constrained, national).

% Seek to revise the Constitution through democratic majority preference. Face the barrier that constitutional amendment requires supermajority consensus and that the Court may invalidate proposed amendments as inconsistent with judicially-discovered unamendable principles. The ordinary legislative process cannot reach constitutional revision; the extraordinary amendment process is gatekept by the judiciary. This group bears the cost that popular sovereignty is constrained by judicial supremacy.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, popular_sovereignty_advocates, payer,
    organized, biographical, constrained, national).

% Benefit from the clarity and stability provided by Supreme Court precedent. Bound by hierarchy to follow the Supreme Court's constitutional interpretation. Have no independent authority to revise or contest that interpretation. Locked into the supremacy structure—cannot claim co-equal interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, lower_courts, beneficiary,
    institutional, generational, trapped, national).

% Analyze and debate whether the judicial supremacy reading is the correct interpretation of the constitutional text. Document that alternative readings (coordinate construction, parliamentary primacy) are historically and doctrinally defensible. Can publish, teach, and influence discourse without institutional power, but have no formal authority over constitutional interpretation. Can leave the discipline if dissatisfied but retain intellectual autonomy.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_scholars, observer,
    moderate, biographical, mobile, national).

% Gain access to a final arbiter who will resolve their constitutional rights questions authoritatively. Also bound by that interpretation—a narrow reading of a right cannot be revised through legislative override, only through subsequent litigation and possible reversal of precedent (rare and costly). Lack standing to challenge the Court's reading from outside the litigation structure. Trapped within the dispute resolution system that the Court controls.
narrative_ontology:constraint_stakeholder(constitutional_authority_boundary__judicial_supremacy_reading, litigants, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_authority_boundary__judicial_supremacy_reading, litigants, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_authority_boundary__judicial_supremacy_reading, supreme_court_institution).
narrative_ontology:fixing_cost_class(constitutional_authority_boundary__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, final resolver of constitutional disputes: when the legislature, executive, or private parties disagree about what the Constitution requires, the Court's interpretation is binding and final. This prevents endless re-litigation of the same question and gives all parties a clear answer to what is constitutionally permissible—a genuine coordination benefit. Eliminates the chaos that would result if each branch claimed independent authority to interpret the Constitution.
% TRANSFER_FUNCTION: Transfers interpretive authority from distributed (each branch interpreting constitutionally within its sphere) to concentrated (the Supreme Court interpreting for all branches). Transfers the cost of judicial gatekeeping to the legislature and executive (they must get Court approval for their decisions). Transfers the power to revise constitutional meaning from popular majorities to the Court and judges (amendments must pass supermajority AND satisfy judicial gatekeeping).
% ABSENT_VOICES: Proponents of the coordinate-construction reading (the legislature and executive as co-equal constitutional interpreters) are excluded from this reading's framing; they are not given standing in the supremacy doctrine. Advocates for parliamentary primacy (elected representatives as ultimate interpreters) are excluded. Popular majorities who wish to amend the Constitution without supermajority consensus or without fear of judicial invalidation are excluded—they have no institutional voice. Framers and founding-era actors are dead. International constitutional scholars from jurisdictions with different readings (UK, Canada, Nordic countries) are foreign and lack institutional standing in US constitutional interpretation.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_authority_boundary__judicial_supremacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(constitutional_authority_boundary__judicial_supremacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_authority_boundary__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_authority_boundary__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.68 at interval end) captures the asymmetry: the judiciary receives monopoly interpretive power; the legislature and executive receive veto; popular majorities receive a constitutional revision path so constrained (supermajority + judicial gatekeeping) that amendment is prohibitively costly for most political actors. The suppression metric (0.71) reflects active enforcement: the Court must continuously enforce that it is the final arbiter by denying competing interpretations from other branches—any acknowledgment that the legislature could interpret constitutionally within its sphere would undermine the supremacy monopoly. The theater ratio (0.42) reflects that while there is real judicial review function (genuine coordination benefit in settling disputes), a rising share of the Court's work involves defending its own institutional supremacy against legislative or popular challenges (performative maintenance of the monopoly). The measurement series show extraction accumulating over 250 years: early in the period (t=0), the Court's supremacy claim was weaker and faced stronger institutional resistance from co-equal branches; over time, the supremacy reading became institutionalized and resistance waned, allowing extraction to concentrate. Accessibility collapse is leveled: structural-level alternatives (coordinate construction, parliamentary primacy) are nearly foreclosed by doctrine and institutional structure (0.82 at t=250), but individual litigants still retain routes to contest interpretation through new cases (0.65 at t=250). Stakes inflate dramatically: the structural consequences of a wrong reading are regime-destabilizing (organizational level stakes are very high), but individual stakeholders may escape high stakes by conforming behavior. Resistance declines over time as the supremacy reading becomes taken-for-granted.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, judicial supremacy solves a coordination problem: it prevents endless institutional conflict over constitutional meaning and provides a final word that all branches must respect. The judicial seat should compute as seeing a genuine rope or low-extraction arrangement—a necessary clarity mechanism. From the legislature's seat, the arrangement is extractive: policy vetoed after passage, constitutional interpretation monopolized, amendment blocked by judicial gatekeeping—the legislative seat should compute as seeing high extraction, suppression, and snare-like dynamics. From the popular-sovereignty seat, the arrangement is the most extractive: majorities cannot constitutionally revise their fundamental law through ordinary means. The engine should compute these divergences from the structural data (judiciary as beneficiary/agenda_setter, legislature and executive as victims/payers, popular sovereignty advocates as payers). The commentary's perspectival gap explanation is that the same arrangement operates as coordination from one institutional seat and extraction from another—the asymmetry is not measurement error but structural reality. Directionality should differentiate sharply: judiciary near 0.0 (full beneficiary), legislature near 0.7 (substantial target), executive near 0.75 (substantial target), populace near 0.8 (target, but diffuse).
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary is the only institutional actor that benefits from concentrating interpretive authority (d ≈ 0.1–0.2: benefits from the monopoly, low exit pressure). The legislature is a target: it pays through constrained policy space, veto risk, and inability to override through ordinary legislation (d ≈ 0.70: high structural extraction, constrained exit). The executive is similarly targeted (d ≈ 0.75: must implement, cannot contest, trapped). Popular majorities seeking constitutional revision are heavily targeted (d ≈ 0.80: the amendment process is their only exit and it is maximally constrained by judicial gatekeeping). Lower courts are beneficiaries of stability (d ≈ 0.2: they benefit from hierarchical clarity, but are locked in—trapped exit pushes d upward toward 0.5). Constitutional scholars and international observers have high exit options (mobile, analytical) so their d is near 0.5 even though they are critical. The narrative anchors these differences: judges gain rents from the monopoly; branches that lose policymaking authority to veto lose dramatically; the people who wanted constitutional revision through majoritarian means face a closed door.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint was founded to solve a real coordination problem: early constitutional disputes risked institutional deadlock if no final interpreter existed. The founding problem is CONTESTED because alternative democracies (UK, Canada, Nordic countries) have functioned well with coordinate construction or parliamentary primacy readings, suggesting the founding problem admits multiple solutions. Over 250 years, the supremacy reading accumulated extraction: the coordination benefit (a clear, final answer to 'what does the constitution mean?') is real, but the extraction concentrated (interpretation monopoly prevents legislative reversal, amendment is blocked by judicial gatekeeping). The theater ratio rose from 0.20 to 0.42: early enforcement of supremacy was partly about genuine clarity; modern enforcement increasingly defends the institutional monopoly itself against legislative or popular challenges (Goodhart drift—the goal of 'final interpretation' has migrated to 'institutional supremacy'). The mandatrophy threshold is crossed: the founding problem is dead in many democracies, but the arrangement persists through institutional inertia and because the beneficiary (judiciary) has captured the authority structure. However, this is not a piton—extraction is still substantial (0.68), not negligible—so it remains a tangled rope with high extraction and active enforcement, not a piton whose function has atrophied entirely. The reading is therefore coherent: the claim is tangled rope, the metrics reflect tangled rope, and the mandate has partially atrophied but the extraction remains high because the institutional beneficiary continuously works to maintain supremacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    text_vs_precedent_divergence,
    'Does the constitutional text actually establish judicial supremacy, or does judicial supremacy rest on centuries of Court precedent asserting the claim, creating a self-validating reading?',
    'Originalist historical analysis of the founding-era understanding; comparison with the text''s silence on who interprets it; examination of founding-era disputes where branches claimed co-equal interpretive authority.',
    'If supremacy rests primarily on precedent rather than text, the constraint is more clearly extractive (the beneficiary has written itself into authority through institutional practice). If the text genuinely establishes supremacy, the constraint is more clearly a coordination mechanism the founders designed. If the text is ambiguous or silent, the reading is one among defensible alternatives—this classification sits directly on the result.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(text_vs_precedent_divergence, conceptual, 'Whether judicial supremacy is textually established or institutionally self-created through precedent.').

omega_variable(
    coordinate_construction_viability,
    'Could a coordinate-construction reading be institutionally stable, or does institutional equilibrium necessarily push toward concentrating authority in one branch?',
    'Comparative institutional analysis of functioning democracies with coordinate readings (Canada''s dialogue model, legislative review of constitutional interpretation in some parliaments); historical periods in which US branches operated as co-equal interpreters.',
    'If coordinate construction is viable, judicial supremacy is revealed as a contingent choice, not an inevitable solution to the founding problem. If institutional forces necessarily concentrate authority, supremacy may be the stable equilibrium and the extraction is a side effect of a deeper structural necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordinate_construction_viability, empirical, 'Whether coordinate constitutional interpretation can remain stable or tends toward concentration.').

omega_variable(
    amendment_gatekeeping_legitimacy,
    'Is it legitimate for courts to strike down constitutional amendments as inconsistent with ''unamendable'' constitutional principles, thereby gatekeeping popular sovereignty?',
    'Normative constitutional theory on the scope of amendment power; empirical examination of whether courts have actually blocked amendments this way and under what reasoning.',
    'If amendment gatekeeping is legitimate, popular constitutional revision is constrained by design, and the extraction is acceptable as part of judicial guardianship. If it is not legitimate, judicial gatekeeping is pure extraction—the Court blocks the people''s ability to revise their own fundamental law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gatekeeping_legitimacy, preference, 'Whether judicial review of constitutional amendments is a valid institutional function or an illegitimate usurpation of popular sovereign authority.').

omega_variable(
    other_readings_foreclosure,
    'Does the supremacy reading of the constitutional text logically foreclose the coordinate-construction reading, or are both consistent with the text read in good faith?',
    'Textual analysis comparing how supporters of each reading justify their interpretation from the same constitutional provisions; examination of whether either reading claims the text explicitly rules out the other.',
    'If supremacy forecloses coordinate construction, the relation is ''forecloses'' (rare, structural contradiction). If both readings coexist as defensible interpretations of an ambiguous or silent text, the relation is ''coexists_with''. This determines the cs_structure.reading_relations classification and affects the scope of the foreclosure omega.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(other_readings_foreclosure, conceptual, 'Whether coordinate construction is logically incompatible with the judicial supremacy reading or a coexisting alternative interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_authority_boundary__judicial_supremacy_reading, 0, 250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 50, 0.26).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t100, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 100, 0.32).
narrative_ontology:measurement_basis(cons_tr_t100, observed).
narrative_ontology:measurement(cons_tr_t150, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement_basis(cons_tr_t150, observed).
narrative_ontology:measurement(cons_tr_t200, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 200, 0.41).
narrative_ontology:measurement_basis(cons_tr_t200, observed).
narrative_ontology:measurement(cons_tr_t250, constitutional_authority_boundary__judicial_supremacy_reading, theater_ratio, 250, 0.42).
narrative_ontology:measurement_basis(cons_tr_t250, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 50, 0.52).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t100, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 100, 0.58).
narrative_ontology:measurement_basis(cons_be_t100, observed).
narrative_ontology:measurement(cons_be_t150, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 150, 0.64).
narrative_ontology:measurement_basis(cons_be_t150, observed).
narrative_ontology:measurement(cons_be_t200, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 200, 0.66).
narrative_ontology:measurement_basis(cons_be_t200, observed).
narrative_ontology:measurement(cons_be_t250, constitutional_authority_boundary__judicial_supremacy_reading, base_extractiveness, 250, 0.68).
narrative_ontology:measurement_basis(cons_be_t250, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 50, 0.6).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t100, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 100, 0.64).
narrative_ontology:measurement_basis(cons_su_t100, observed).
narrative_ontology:measurement(cons_su_t150, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 150, 0.68).
narrative_ontology:measurement_basis(cons_su_t150, observed).
narrative_ontology:measurement(cons_su_t200, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 200, 0.7).
narrative_ontology:measurement_basis(cons_su_t200, observed).
narrative_ontology:measurement(cons_su_t250, constitutional_authority_boundary__judicial_supremacy_reading, suppression_requirement, 250, 0.71).
narrative_ontology:measurement_basis(cons_su_t250, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=250
narrative_ontology:measurement(cons_grid_01, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(class), 0, 0.58).
narrative_ontology:measurement(cons_grid_02, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(class), 250, 0.74).
narrative_ontology:measurement(cons_grid_03, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(individual), 0, 0.5).
narrative_ontology:measurement(cons_grid_04, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(individual), 250, 0.65).
narrative_ontology:measurement(cons_grid_05, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(organizational), 0, 0.65).
narrative_ontology:measurement(cons_grid_06, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(organizational), 250, 0.78).
narrative_ontology:measurement(cons_grid_07, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(structural), 0, 0.72).
narrative_ontology:measurement(cons_grid_08, constitutional_authority_boundary__judicial_supremacy_reading, accessibility_collapse(structural), 250, 0.82).
narrative_ontology:measurement(cons_grid_09, constitutional_authority_boundary__judicial_supremacy_reading, resistance(class), 0, 0.48).
narrative_ontology:measurement(cons_grid_10, constitutional_authority_boundary__judicial_supremacy_reading, resistance(class), 250, 0.58).
narrative_ontology:measurement(cons_grid_11, constitutional_authority_boundary__judicial_supremacy_reading, resistance(individual), 0, 0.4).
narrative_ontology:measurement(cons_grid_12, constitutional_authority_boundary__judicial_supremacy_reading, resistance(individual), 250, 0.55).
narrative_ontology:measurement(cons_grid_13, constitutional_authority_boundary__judicial_supremacy_reading, resistance(organizational), 0, 0.55).
narrative_ontology:measurement(cons_grid_14, constitutional_authority_boundary__judicial_supremacy_reading, resistance(organizational), 250, 0.52).
narrative_ontology:measurement(cons_grid_15, constitutional_authority_boundary__judicial_supremacy_reading, resistance(structural), 0, 0.62).
narrative_ontology:measurement(cons_grid_16, constitutional_authority_boundary__judicial_supremacy_reading, resistance(structural), 250, 0.45).
narrative_ontology:measurement(cons_grid_17, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(class), 0, 0.45).
narrative_ontology:measurement(cons_grid_18, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(class), 250, 0.62).
narrative_ontology:measurement(cons_grid_19, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(individual), 0, 0.35).
narrative_ontology:measurement(cons_grid_20, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(individual), 250, 0.5).
narrative_ontology:measurement(cons_grid_21, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(organizational), 0, 0.55).
narrative_ontology:measurement(cons_grid_22, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(organizational), 250, 0.7).
narrative_ontology:measurement(cons_grid_23, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(structural), 0, 0.6).
narrative_ontology:measurement(cons_grid_24, constitutional_authority_boundary__judicial_supremacy_reading, stakes_inflation(structural), 250, 0.75).
narrative_ontology:measurement(cons_grid_25, constitutional_authority_boundary__judicial_supremacy_reading, suppression(class), 0, 0.5).
narrative_ontology:measurement(cons_grid_26, constitutional_authority_boundary__judicial_supremacy_reading, suppression(class), 250, 0.66).
narrative_ontology:measurement(cons_grid_27, constitutional_authority_boundary__judicial_supremacy_reading, suppression(individual), 0, 0.45).
narrative_ontology:measurement(cons_grid_28, constitutional_authority_boundary__judicial_supremacy_reading, suppression(individual), 250, 0.6).
narrative_ontology:measurement(cons_grid_29, constitutional_authority_boundary__judicial_supremacy_reading, suppression(organizational), 0, 0.55).
narrative_ontology:measurement(cons_grid_30, constitutional_authority_boundary__judicial_supremacy_reading, suppression(organizational), 250, 0.72).
narrative_ontology:measurement(cons_grid_31, constitutional_authority_boundary__judicial_supremacy_reading, suppression(structural), 0, 0.5).
narrative_ontology:measurement(cons_grid_32, constitutional_authority_boundary__judicial_supremacy_reading, suppression(structural), 250, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_authority_boundary__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_authority_boundary__judicial_supremacy_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__coordinate_construction_reading).
narrative_ontology:affects_constraint(constitutional_authority_boundary__judicial_supremacy_reading, constitutional_authority_boundary__parliamentary_primacy_reading).

% DUAL FORMULATION NOTE:
% The constitutional-authority-boundary kernel decomposes into three structurally distinct constraint stories, one per reading. Judicial supremacy (this story) establishes the Court as final arbiter; coordinate construction vests each branch with co-equal interpretive authority; parliamentary primacy vests ultimate authority in the elected legislature. Each reading produces a different ε (supremacy is ~0.68 extractive; coordinate construction is ~0.35 as pure coordination; parliamentary primacy is ~0.52 as delegated-but-revocable), different beneficiary/victim sets, and different type classifications. The three stories form a kernel family linked via network.affects_constraints: each reading is a different way of organizing the same constitutional text. The judicial supremacy story influences the other two because when the supremacy reading is institutionalized (as it historically was in the US), it structurally constrains the viability of coordinate construction and parliamentary primacy readings in that polity, even though both readings remain theoretically live in other democracies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(constitutional_authority_boundary__judicial_supremacy_reading, organized, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
