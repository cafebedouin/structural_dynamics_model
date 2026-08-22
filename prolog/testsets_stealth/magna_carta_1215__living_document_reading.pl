% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta Living-Document Interpretive Regime
 *   domain: constitutional law/legal history/political theory
 *
 * SUMMARY:
 *   This story authors the living-document reading of Magna Carta as a clean,
 *   epsilon-invariant constraint: the standing arrangement under contest is
 *   the interpretive-authority regime itself, by which accumulated tradition
 *   legitimately supersedes the 1215 original meaning and precedential
 *   accumulation counts as constitutional development. The regime genuinely
 *   coordinates (one authoritative, continuously updated answer to what an
 *   unamendable ancient text requires) and simultaneously transfers
 *   interpretive authority from enactors and legislatures to benches and the
 *   profession, under active enforcement (courts bind by their readings; the
 *   profession polices method). The claim/metric gap is deliberate: the
 *   reading CLAIMS tangled_rope from its own endorsing seat while the metrics
 *   are authored independently as descriptively true; the engine computes
 *   per-seat classifications from the structural data. Family membership:
 *   this is one of three readings of kernel magna_carta_1215; the sibling
 *   stories carry the other two constraints, linked via
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - appellate_judiciary: agenda-setter (institutional/identity_locked) — administers the interpretive regime; its authority is constituted by the tradition it maintains
 *   - constitutional_bar_and_legal_academy: primary beneficiary (powerful/arbitrage) — collects fees, careers, and prestige scaled to interpretive volume
 *   - rights_claimants_invoking_evolved_meaning: secondary beneficiary (powerless/trapped) — receives protections only the developed reading supplies
 *   - original_meaning_adherents: primary payer (organized/constrained) — bears methodological displacement inside forums premised against them
 *   - democratic_legislative_majorities: payer (institutional/constrained) — loses statutory outcomes to meaning they never enacted
 *   - general_citizens_subject_to_unenacted_limits: diffuse payer (powerless/trapped) — governed by doctrinal content they never consented to
 *   - lay_public_outside_interpretive_conversation: excluded (powerless/trapped) — bound by, but absent from, doctrinal development
 *   - constitutional_historians: analytical observer — attests the record from outside the profession's stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.4).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.35).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta Living-Document Interpretive Regime").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional law/legal history/political theory").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '66748976-553b-4ebb-abba-ab7508fbe827').
narrative_ontology:cs_kernel_codification('66748976-553b-4ebb-abba-ab7508fbe827', fixed_text).
narrative_ontology:cs_authority_grounding('66748976-553b-4ebb-abba-ab7508fbe827', lineage).
narrative_ontology:cs_interpretation_layer_present('66748976-553b-4ebb-abba-ab7508fbe827').
narrative_ontology:cs_reading_relation('66748976-553b-4ebb-abba-ab7508fbe827', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('66748976-553b-4ebb-abba-ab7508fbe827', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_axiom('66748976-553b-4ebb-abba-ab7508fbe827', foundational, interpretive_tradition_supersedes_original_meaning).
narrative_ontology:cs_axiom_status(interpretive_tradition_supersedes_original_meaning, holdable).
narrative_ontology:cs_axiom_grounding('66748976-553b-4ebb-abba-ab7508fbe827', interpretive_tradition_supersedes_original_meaning, conventional).
narrative_ontology:cs_axiom('66748976-553b-4ebb-abba-ab7508fbe827', secondary, precedential_accumulation_constitutes_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('66748976-553b-4ebb-abba-ab7508fbe827', precedential_accumulation_constitutes_development, instrumental).
narrative_ontology:cs_reference_frame('66748976-553b-4ebb-abba-ab7508fbe827', adaptive_interpretive_continuity).
narrative_ontology:cs_drift_state('66748976-553b-4ebb-abba-ab7508fbe827', contemporary_originalist_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('66748976-553b-4ebb-abba-ab7508fbe827', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, appellate_judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_bar_and_legal_academy).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, rights_claimants_invoking_evolved_meaning).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, original_meaning_adherents).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, democratic_legislative_majorities).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, general_citizens_subject_to_unenacted_limits).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, stare_decisis_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, common_law_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits atop the interpretive hierarchy: decides what the Charter's clauses require under present conditions, and each decision joins the body of accumulated meaning that future benches inherit and extend. Tenure insulates members from removal, but their authority rests entirely on the tradition they maintain; stepping outside the accumulative method would dissolve the basis of their own office. Exit from the role is practically unavailable, and the judicial identity is formed inside the practice it administers.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, appellate_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).

% Argues, teaches, and writes the doctrine: counsel bill constitutional litigation, professors build careers explicating the accumulated gloss, and the profession staffs the clerkships and commissions that feed the tradition. Members move fluidly between practice, academy, and bench, and their livelihoods scale with the volume and complexity of interpretive work the tradition generates.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_bar_and_legal_academy, beneficiary,
    powerful, biographical, arbitrage, global).

% Come to court seeking protections the 1215 text nowhere names, such as procedural fairness beyond its medieval forms, and receive them only because the accumulated reading has grown past the original words. Without the developed tradition their claims have no textual home, and no other institution will supply the equivalent protection.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, rights_claimants_invoking_evolved_meaning, beneficiary,
    powerless, immediate, trapped, national).

% Judges, scholars, and advocates who hold that the Charter's clauses mean what the contracting parties understood in 1215 and that later accretions lack binding force. They litigate inside courts whose method presumes the opposite, publish in venues the tradition credentials, and see their positions lose not for want of talent but because the forum's premises exclude them. Formal amendment is their stated alternative and is rarely reachable.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, original_meaning_adherents, payer,
    organized, biographical, constrained, national).

% Enact statutes that courts sometimes set aside by appeal to developed constitutional meaning the legislature never voted on. The formal amendment path exists but demands supermajorities and multi-year process no ordinary majority can muster, so the practical responses are appointment politics and jurisdiction-limiting bills: slow counters to fast-moving doctrine.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, democratic_legislative_majorities, payer,
    institutional, generational, constrained, national).

% Live under limits on what their legislatures may do that originate in judicial readings of an eight-century-old text. Many receive real protections this way; none consented to the specific contents of the accumulated gloss, and emigration or exit is not a realistic response to a doctrine one dislikes.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, general_citizens_subject_to_unenacted_limits, payer,
    powerless, civilizational, trapped, national).

% Ratified nothing and argues nothing: doctrinal development happens in judgments, law reviews, and closed conferences. Public ceremonies reaffirm the Charter at anniversaries while the operative meaning is set elsewhere; the public's participation amounts to celebrating a document whose working content it did not shape.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, lay_public_outside_interpretive_conversation, excluded,
    powerless, generational, trapped, national).

% Study the documentary record of 1215 and its reception from outside the profession's argumentative stakes. They attest what the original settlement said, trace how each era's reading served that era's needs, and supply the evidence that both defenders and critics of the accumulative method cite.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, appellate_judiciary).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one authoritative, continuously updated answer to what the Charter requires under present conditions, letting an unamendable ancient text keep doing constitutional work across radically changed circumstances without recurring legitimacy crises.
% TRANSFER_FUNCTION: Moves interpretive authority from the text's original enactors and from sitting legislative majorities to successive benches and the legal profession; moves concrete limits on public power from enacted statute to adjudicated precedent.
% ABSENT_VOICES: The lay public bound by unratified doctrinal content, and originalist legislators whose methodological position receives no hearing inside the courtroom. Both stand outside the conversation where operative meaning is made; their objections surface only in political channels that cannot reach the doctrine directly.
% DISAPPEARANCE_RATIONALE: Overnight removal would strand eight centuries of developed meaning: every protection the courts now enforce beyond the 1215 text would lose its warrant simultaneously, the profession's argumentative practice would lose its object, and the polity would face a frozen text governing changed conditions, forcing either an amendment crisis or abandonment of the text. Arrangements across the common-law world depend on the regime continuing.
% FOUNDING_PROBLEM: An ancient charter, granted to constrain a medieval king, had to keep governing polities and problems its drafters could not imagine; some mechanism was needed to let the text's authority persist while its content changed.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary set attest the adaptation problem is real: the documentary record shows literal-application efforts failing within a generation of the grant. Originalist jurists corroborate the problem's existence while disputing the accumulative solution, and legislative debate records repeatedly acknowledge that formal amendment alone cannot carry the load. Corroboration of the problem crosses the beneficiary boundary; the adequacy of this regime as the answer remains disputed by those same outside voices.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.4, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is authored at 0.40 from this reading's own seat: the regime's costs are real (authority concentration, democratic displacement, unratified doctrinal content) but bounded by a load-bearing coordination function, so the endorsing reading assesses moderate rather than severe extraction. Suppression 0.35 is a raw structural property, unscaled by power or scope: the courtroom's method monopoly is enforced, with an internalized component (professional socialization into precedent-deference) flagged in the stare_decisis_internalization omega. Theater 0.30 reflects anniversary ceremonialism and symbolic reaffirmation that increasingly diverges from operative content. Accessibility_collapse 0.35: originalism and formal amendment remain live alternatives, so alternatives only partly collapse. Resistance 0.55: a sustained originalist and political counter-movement actively contests the regime. The measurement series run on one shared seven-point grid (every tracked metric authored at every time point); trajectories rise monotonically with the accumulation dynamic rather than cycling. Enforcement history is traced via suppression_requirement because the story's dynamic is the maturation of the interpretive enforcement machinery alongside growing divergence from original meaning.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from identical structural inputs. From the bench, the regime is stewardship: continuity maintained, adaptation delivered, legitimacy renewed each generation. From the bar and academy, it is opportunity: careers and fees scaling with doctrinal volume. From the claimant seat, it is rescue: protections unavailable anywhere else. From the originalist seat, it is methodological exclusion: losing inside forums whose premises preclude the argument. From the legislative seat, it is displacement of enacted law by unenacted meaning. Same-power differentiation: the bar and originalist scholars occupy the same nominal professional class with opposite exits, because one sells the tradition and the other disputes it; arbitrage versus constrained exit follows from that relationship to the doctrine, not from global standing.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the judiciary, bar, and claimants; victim declarations drive high directionality for original-meaning adherents, legislative majorities, and citizens. The judiciary's dual position (agenda-setter and beneficiary) is captured by role plus secondary positioning in the structural data rather than an override, because the derivation from beneficiary-plus-administration already yields the correct near-beneficiary d. General citizens are the one genuinely mixed seat: the reading itself holds that diffuse evolved protections offset much of the unenacted-limits cost, so their true d sits nearer symmetric than the victim declaration alone suggests; this is documented here rather than overridden, since the aggregate derivation remains serviceable and the mixture is a matter of degree the engine's scope handling can absorb.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (an ancient text must govern changed conditions) is live, so no mandatrophy is declared and no sunset applies. Classification prevents two symmetrical mislabels: reading the regime as pure extraction ignores that the coordination function is genuine and load-bearing (remove it and the constitution stops working overnight, per the disappearance verdict); reading it as pure coordination ignores the enforced, asymmetric transfer of interpretive authority to a self-perpetuating professional class. Tangled_rope holds both truths: coordinated and paying through the same structure, held together by active enforcement. The identity-lock on the bench matters here: because judicial identity fuses with the interpretive office, the regime's administrators cannot neutrally evaluate it, which is precisely the mechanism by which a coordination structure accumulates extraction without anyone deciding to extract.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading of kernel magna_carta_1215 (the living_document_reading): how would instantiating a sibling reading instead change the structural picture?',
    'Generate the sibling stories (baronial_privilege_reading, universal_rights_reading) and compare beneficiary/victim sets, epsilon, and authority structure across the family.',
    'The baronial reading shrinks the beneficiary set to landed contracting parties and dates the arrangement''s death centuries ago; the universal reading enlarges the protected class to all persons and raises measured extraction by counting everyone the developed meaning still fails to protect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame position: one of three readings of the Magna Carta kernel.').

omega_variable(
    authority_locus_disagreement,
    'Where do the three readings locate the authority that fixes the Charter''s meaning: the 1215 contracting parties'' intent, a transhistorical principle, or the accumulating interpretive tradition?',
    'Conceptual analysis of what each reading treats as defeasible: a reading that lets the tradition err against original intent places authority in the intent; one that lets the text err against principle places authority in the principle; this reading treats neither as defeasible against the tradition and locates authority there.',
    'Relocating authority to original intent collapses this constraint toward the baronial reading''s victim structure; relocating it to transhistorical principle merges it toward the universal reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_locus_disagreement, conceptual, 'The specific structural element on which the sibling readings diverge: the locus of interpretive authority.').

omega_variable(
    democratic_deficit_alignment,
    'Does doctrinal development track the values of the publics bound by it, or the values of the professional class that produces it?',
    'Longitudinal comparison of landmark doctrinal shifts against contemporaneous legislative preference and survey data.',
    'Systematic misalignment would push effective extraction above the authored 0.40 and shift the computed type toward capture; alignment would support the coordination-dominant reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_deficit_alignment, empirical, 'Whether the accumulative regime serves governed publics or the producing profession.').

omega_variable(
    amendment_counterfactual,
    'Could the constitution have adapted through formal amendment alone, making the accumulative regime''s costs unnecessary?',
    'Historical throughput analysis: amendment rates in comparable rigid constitutions versus the pace of adaptation the accumulated reading actually delivered.',
    'If amendment could not have kept pace, much of the measured transfer is the price of adaptability rather than discretionary capture; if it could, the regime''s persistence reflects professional advantage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_counterfactual, empirical, 'Counterfactual necessity of the accumulative mechanism.').

omega_variable(
    stare_decisis_internalization,
    'Is adherence to accumulated precedent structural (enforceable rules) or internalized (professional identity renders deviation unthinkable)?',
    'Observe benches whose composition is hostile to the tradition: if they still extend precedent, the bind is internalized; if they distinguish it away case by case, the bind is structural.',
    'Internalized adherence survives formal reform, so measured suppression would persist after any enforcement change; structural adherence tracks the enforcement machinery directly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stare_decisis_internalization, empirical, 'Structural versus internalized mechanism behind precedent-deference.').

omega_variable(
    divergence_ceiling,
    'Is there a limit to how far accumulated meaning may drift from the text before the legitimacy claim breaks?',
    'Comparative analysis: identify constitutional traditions where the gap between honored text and operative meaning exceeded tolerance and the framework was discarded or formally rewritten.',
    'A ceiling bounds the rising trajectories in the measurement series; no ceiling implies continued growth in extraction past the interval end.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divergence_ceiling, empirical, 'Whether the accumulation dynamic has a legitimacy bound.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 810).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc_living_doc_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement_basis(mc_living_doc_tr_t0, observed).
narrative_ontology:measurement(mc_living_doc_tr_t82, magna_carta_1215__living_document_reading, theater_ratio, 82, 0.06).
narrative_ontology:measurement_basis(mc_living_doc_tr_t82, observed).
narrative_ontology:measurement(mc_living_doc_tr_t400, magna_carta_1215__living_document_reading, theater_ratio, 400, 0.12).
narrative_ontology:measurement_basis(mc_living_doc_tr_t400, observed).
narrative_ontology:measurement(mc_living_doc_tr_t561, magna_carta_1215__living_document_reading, theater_ratio, 561, 0.16).
narrative_ontology:measurement_basis(mc_living_doc_tr_t561, observed).
narrative_ontology:measurement(mc_living_doc_tr_t650, magna_carta_1215__living_document_reading, theater_ratio, 650, 0.21).
narrative_ontology:measurement_basis(mc_living_doc_tr_t650, observed).
narrative_ontology:measurement(mc_living_doc_tr_t750, magna_carta_1215__living_document_reading, theater_ratio, 750, 0.26).
narrative_ontology:measurement_basis(mc_living_doc_tr_t750, observed).
narrative_ontology:measurement(mc_living_doc_tr_t810, magna_carta_1215__living_document_reading, theater_ratio, 810, 0.3).
narrative_ontology:measurement_basis(mc_living_doc_tr_t810, observed).

% Extraction over time
narrative_ontology:measurement(mc_living_doc_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement_basis(mc_living_doc_be_t0, observed).
narrative_ontology:measurement(mc_living_doc_be_t82, magna_carta_1215__living_document_reading, base_extractiveness, 82, 0.08).
narrative_ontology:measurement_basis(mc_living_doc_be_t82, observed).
narrative_ontology:measurement(mc_living_doc_be_t400, magna_carta_1215__living_document_reading, base_extractiveness, 400, 0.17).
narrative_ontology:measurement_basis(mc_living_doc_be_t400, observed).
narrative_ontology:measurement(mc_living_doc_be_t561, magna_carta_1215__living_document_reading, base_extractiveness, 561, 0.24).
narrative_ontology:measurement_basis(mc_living_doc_be_t561, observed).
narrative_ontology:measurement(mc_living_doc_be_t650, magna_carta_1215__living_document_reading, base_extractiveness, 650, 0.31).
narrative_ontology:measurement_basis(mc_living_doc_be_t650, observed).
narrative_ontology:measurement(mc_living_doc_be_t750, magna_carta_1215__living_document_reading, base_extractiveness, 750, 0.36).
narrative_ontology:measurement_basis(mc_living_doc_be_t750, observed).
narrative_ontology:measurement(mc_living_doc_be_t810, magna_carta_1215__living_document_reading, base_extractiveness, 810, 0.4).
narrative_ontology:measurement_basis(mc_living_doc_be_t810, observed).

% Suppression requirement over time
narrative_ontology:measurement(mc_living_doc_su_t0, magna_carta_1215__living_document_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement_basis(mc_living_doc_su_t0, observed).
narrative_ontology:measurement(mc_living_doc_su_t82, magna_carta_1215__living_document_reading, suppression_requirement, 82, 0.05).
narrative_ontology:measurement_basis(mc_living_doc_su_t82, observed).
narrative_ontology:measurement(mc_living_doc_su_t400, magna_carta_1215__living_document_reading, suppression_requirement, 400, 0.14).
narrative_ontology:measurement_basis(mc_living_doc_su_t400, observed).
narrative_ontology:measurement(mc_living_doc_su_t561, magna_carta_1215__living_document_reading, suppression_requirement, 561, 0.21).
narrative_ontology:measurement_basis(mc_living_doc_su_t561, observed).
narrative_ontology:measurement(mc_living_doc_su_t650, magna_carta_1215__living_document_reading, suppression_requirement, 650, 0.27).
narrative_ontology:measurement_basis(mc_living_doc_su_t650, observed).
narrative_ontology:measurement(mc_living_doc_su_t750, magna_carta_1215__living_document_reading, suppression_requirement, 750, 0.32).
narrative_ontology:measurement_basis(mc_living_doc_su_t750, observed).
narrative_ontology:measurement(mc_living_doc_su_t810, magna_carta_1215__living_document_reading, suppression_requirement, 810, 0.35).
narrative_ontology:measurement_basis(mc_living_doc_su_t810, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, universal_rights_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Magna Carta' covers three structurally distinct constraints instantiated by three readings of one kernel: the baronial_privilege_reading (a contract among landed parties), this living_document_reading (an interpretive-authority regime whose epsilon referent is the accumulative tradition itself), and the universal_rights_reading (Clause 39 as a universal due-process constraint). Each carries its own epsilon, beneficiary/victim structure, and classification. This file authors only the living-document reading and links the family via affects_constraints; the upstream accumulative machinery legitimized here is the vehicle through which the universal reading's expansion traveled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
