% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Constitutional Interpretation Doctrine
 *   domain: constitutional_law/legal_philosophy
 *
 * SUMMARY:
 *   Originalism as a constitutional doctrine claims that the Constitution's
 *   meaning is fixed at ratification and that interpretation must recover the
 *   original public understanding of the text. This constraint is ONE READING
 *   of the contested kernel 'us_constitution_text'. It is structurally
 *   distinct from living-constitutionalist and positivist readings because it
 *   privileges historical evidence over contemporary needs and chains
 *   judicial authority to historical fact. The constraint operates as both
 *   coordination mechanism (fixing meaning prevents arbitrary revision) and
 *   extraction mechanism (suppresses rights claims and interpretive
 *   frameworks that fall outside historical scope). The measurement series
 *   tracks extraction accumulation (0.42 to 0.68 over 50 years) and rising
 *   theater ratio (0.18 to 0.42), indicating the methodology's legitimacy
 *   claims increasingly do rhetorical work relative to actual
 *   constraint-function (Goodhart drift: originalism becomes performative
 *   constitutional fealty rather than limiting principle).
 *
 * KEY AGENTS:
 *   - originalist_legal_movement: agenda-setter (sets and enforces interpretive methodology); institutional power; benefits from institutional dominance
 *   - conservative_institutional_coalition: beneficiary (gains policy predictability anchored to history); powerful; arbitrage exit
 *   - rights_claimants_post_ratification_needs: victim (denied adaptive interpretation access); organized; constrained exit
 *   - adaptive_interpretation_practitioners: victim (marginalized professionally); moderate power; constrained exit
 *   - federal_judiciary: dual-positioned (enforces the doctrine and is bound by it); institutional; constrained
 *   - living_constitutionalist_school: excluded (competing interpretive frame); powerful but trapped by doctrine
 *   - analytical_observer: observes the structure as entanglement of coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.68).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.79).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Constitutional Interpretation Doctrine").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "constitutional_law/legal_philosophy").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, '974fe874-ab19-420b-a582-fc6e5034b89f').
narrative_ontology:cs_kernel_codification('974fe874-ab19-420b-a582-fc6e5034b89f', fixed_text).
narrative_ontology:cs_authority_grounding('974fe874-ab19-420b-a582-fc6e5034b89f', lineage).
narrative_ontology:cs_interpretation_layer_present('974fe874-ab19-420b-a582-fc6e5034b89f').
narrative_ontology:cs_reading_relation('974fe874-ab19-420b-a582-fc6e5034b89f', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('974fe874-ab19-420b-a582-fc6e5034b89f', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('974fe874-ab19-420b-a582-fc6e5034b89f', foundational, meaning_fixed_at_ratification).
narrative_ontology:cs_axiom_status(meaning_fixed_at_ratification, holdable).
narrative_ontology:cs_axiom_grounding('974fe874-ab19-420b-a582-fc6e5034b89f', meaning_fixed_at_ratification, deontological).
narrative_ontology:cs_axiom('974fe874-ab19-420b-a582-fc6e5034b89f', foundational, original_public_understanding_primacy).
narrative_ontology:cs_axiom_status(original_public_understanding_primacy, holdable).
narrative_ontology:cs_axiom_grounding('974fe874-ab19-420b-a582-fc6e5034b89f', original_public_understanding_primacy, empirically_contingent).
narrative_ontology:cs_reference_frame('974fe874-ab19-420b-a582-fc6e5034b89f', historical_fidelity_to_ratification).
narrative_ontology:cs_drift_state('974fe874-ab19-420b-a582-fc6e5034b89f', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('974fe874-ab19-420b-a582-fc6e5034b89f', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, originalist_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_institutional_coalition).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, rights_claimants_post_ratification_needs).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, adaptive_interpretation_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, historical_evidence_scholars).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, democratic_amendment_process).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, constitutional_meaning_stability_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_text__originalist_reading, historical_evidence_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces originalist methodology through judicial appointments, law school pedagogy, and professional prestige allocation. Controls which interpretive frameworks are treated as legitimate. Benefits directly from institutional dominance: conservative policy outcomes (gun rights, limited federal power, restrictive privacy readings) gain presumptive constitutional permanence when anchored to historical meaning.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_legal_movement, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains policy durability through originalist doctrine. Outcomes that would face democratic revision under living-constitutionalist interpretation are locked in as historically mandated. Receives indirect benefits: policy outcomes justified by history rather than preference are politically more sustainable and legally more defensible against future modification.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_institutional_coalition, beneficiary,
    powerful, generational, arbitrage, national).

% Bear the constraint through suppression of claims not grounded in historical practice: privacy in reproductive autonomy, dignity-based equal protection, digital-age free speech, evolving understandings of equality. Their remedies are historical-evidence recovery (nearly impossible) or constitutional amendment (politically intractable). The constraint operates through epistemic exclusion: their evidence (contemporary harm, contemporary need) is ruled out-of-order.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, rights_claimants_post_ratification_needs, payer,
    organized, biographical, constrained, national).

% Marginalized professionally and institutionally. Law professors teaching living-constitutionalism or purposive interpretation face reduced influence and prestige. Judges appointed on non-originalist commitments experience pressure to conform to originalist methodology or face charges of activism. The constraint operates through occupational closure: advancement requires originalist credentials.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, adaptive_interpretation_practitioners, payer,
    moderate, biographical, constrained, national).

% Gain professional prestige, grant funding, and influence over constitutional doctrine by producing historical accounts of original meaning. The constraint creates demand for their labor and positions their work as the ultimate arbiter of constitutional legitimacy. They benefit without running the constraint.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, historical_evidence_scholars, beneficiary,
    moderate, biographical, arbitrage, national).

% Implements and enforces originalist doctrine through case decisions. Judges appointed on originalist commitments are bound by the methodology; departure is institutional betrayal. The judiciary benefits from having a clear limiting principle but is constrained by it — adaptive discretion is eliminated. Courts experience pressure to apply originalism consistently even when outcomes seem unjust.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, federal_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, federal_judiciary, payer).

% Would argue that constitutional meaning must evolve with societal understanding and that adaptive interpretation is legitimate. Their framework is treated as activist and illegitimate by the originalist coalition. They cannot compete on equal methodological terms; they must either adopt originalist language or accept marginalization. Their ability to influence doctrine is structurally suppressed.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_school, excluded,
    powerful, biographical, trapped, national).

% Would argue that constitutional validity derives from formal enactment procedure, not from recovered historical meaning or content interpretation. Their approach is treated as insufficiently principled. They are excluded from primary institutional influence over constitutional interpretation.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, positivist_legal_school, excluded,
    moderate, biographical, trapped, national).

% Is suppressed by originalism as a route to constitutional evolution. Under living-constitutionalism, adaptive interpretation could respond to contemporary needs without amendment. Under originalism, constitutional change requires the almost-impossible amendment process, shifting power away from courts and voters toward historical evidence recovery.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, democratic_amendment_process, payer,
    organized, generational, constrained, national).

% Observes the originalist doctrine as an enforcement mechanism that simultaneously coordinates constitutional expectation (fixing meaning prevents arbitrary revision) and extracts institutional authority from adaptive interpreters and rights claimants (suppresses their frameworks and transfers authority to historical scholars and originalist judges).
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, analytical_observer, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, originalist_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fixed evidentiary frame (original public meaning at ratification) that constrains judicial interpretation and prevents arbitrary constitutional revision. Solves a genuine coordination problem: without a limiting principle, each judge could interpret the Constitution according to contemporary policy preference, generating inconsistent doctrine across circuits and generations. Originalism fixes meaning to prevent that drift.
% TRANSFER_FUNCTION: Moves institutional authority from living-constitutionalist practitioners and contemporary rights claimants to originalist scholars and conservative institutional coalitions. Transfer is achieved through epistemic suppression: contemporary evidence (needs, harms, understanding) is ruled inadmissible; only historical evidence counts. The constraint also transfers power away from the democratic amendment process toward historical evidence recovery — making constitutional change require nearly impossible amendment rather than responsive interpretation.
% ABSENT_VOICES: Living-constitutionalist judges, adaptive-interpretation scholars, and rights claimants not grounded in historical practice would argue that originalism impermissibly locks constitutional meaning into 18th–19th century understanding and prevents courts from interpreting principles (liberty, equality, due process) in light of contemporary circumstances and novel harms. They would argue the constraint suppresses legitimate jurisprudence under the pretense of constraint. They are structurally excluded from institutional influence; their interpretive frameworks are treated as activist and illegitimate by the dominant coalition.
% DISAPPEARANCE_RATIONALE: If originalist constraint vanished, constitutional interpretation would immediately expand to accommodate living-constitutionalist, purposive, and adaptive frames. Rights claimants would gain access to judges willing to interpret liberty and equality adaptively. Conservative outcomes anchored to historical meaning would lose presumptive permanence and would become subject to democratic and judicial reconsideration. The entire legitimacy structure of contemporary conservative constitutionalism would reorganize around different grounds or would require amendment to persist.
% FOUNDING_PROBLEM: In the 1970s–1980s, constitutional interpretation was perceived (by conservative jurisprudence) as excessively responsive to judicial preference and contemporary values. The Warren Court's expansive rights readings and the Burger Court's continued adaptivity were criticized as unconstrained judicial amendment of the Constitution. Originalism was developed as a methodological response: fix meaning at ratification and require judges to recover historical understanding rather than impose contemporary values.
% FOUNDING_PROBLEM_CORROBORATION: Conservative originalist scholars and judges attest the founding problem persists (unconstrained interpretation remains a threat) and that originalism constrains it. Living-constitutionalist scholars and judges attest the founding problem was misdiagnosed — that any interpretive methodology involves judicial discretion, that originalism's discretion (interpreting history) is no more constrained than other methods, and that originalism's real function is to anchor conservative policy outcomes to historical rather than policy rationales. Federal judges attest that originalism provides clarity; judges who departed from it attest it eliminates legitimate tools for constitutional justice. No source independent of the jurisprudential movements themselves attests the founding problem as originally framed, nor attests that originalism solves it. The mounting evidence that originalism has not actually reduced interpretive disagreement or judicial discretion (illustrated by high rates of originalist disagreement in hard cases) suggests the founding problem was either misidentified or remains unsolved.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because originalism operates to suppress interpretive frameworks that would expand constitutional meaning and because its evidentiary gatekeeping (admitting only historical evidence) transfers authority from rights claimants to historical scholars and originalist judges. Suppression (0.79) is substantial and rising because the constraint's persistence depends on actively excluding living-constitutionalist, purposive, and adaptive frames from institutional legitimacy — the exclusion is not external coercion but internal epistemic gatekeeping. Theater ratio (0.42) is moderate-rising, indicating that as originalism became institutionally dominant, increasingly a share of its enforcement involves rhetorical performance (celebrating judicial restraint, performing fidelity to text) rather than actual limitation. The measurement series uses a single shared time grid (interval 0–50 years, matching contemporary originalism's rise from 1970s to 2020s); extractiveness accelerates through institutional consolidation and plateaus once originalism becomes dominant; theater rises as the doctrine becomes ornamental to power-maintenance rather than functionally limiting.
 *
 * PERSPECTIVAL GAP:
 *   From the originalist agenda-setter seat, the constraint is genuine coordination: it fixes meaning and constrains judicial discretion, solving a real problem of interpretive drift. From the rights-claimant and adaptive-interpreter seats, the constraint is primarily extractive: it suppresses their legitimate interpretive frameworks and denies them access to tools for constitutional justice. The federal judiciary sits between: they benefit from the limiting principle (clear methodology) and are harmed by it (eliminated adaptive discretion). The engine computes this divergence from the structural data — the payer and beneficiary seats will show different types when directionality is applied.
 *
 * DIRECTIONALITY LOGIC:
 *   Originalist scholars and conservative institutional coalitions are near the beneficiary end (d ≈ 0.15–0.25): they benefit from the constraint's enforcement and help run it. Rights claimants and adaptive practitioners are near the target end (d ≈ 0.75–0.85): the constraint suppresses their claims and frames them as illegitimate. Federal judges have asymmetric d depending on whether they were appointed on originalist commitments (d ≈ 0.35–0.50, constrained) or were practicing adaptive interpretation before appointment (higher d, forced conformity). The living-constitutionalist and positivist schools are excluded, not targeted — they would compete if they had structural access, but the originalist frame prevents that access.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (unconstrained judicial discretion) was legitimate in the 1970s–1980s. However, originalism has not solved it — it has merely transferred discretion from one source (judge's policy preference) to another (judge's interpretation of historical evidence, which is also contestable and judgment-laden). The constraint persists past its founding function because it benefits originalist institutional coalitions and conservative policy outcomes. Contemporary mandatrophy is evidenced by the gap between originalism's limiting-principle rhetoric and its actual operation as a tool for conservative institutional dominance. The theater ratio rise (0.18 to 0.42) documents this drift: the constraint increasingly operates through performance (celebrating restraint, performing historical fidelity) rather than through actual constraint on interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_evidence_indeterminacy,
    'How much does historical evidence actually determine original meaning, and how much is interpretation-of-evidence itself an exercise of judicial discretion?',
    'Historiographical and jurisprudential analysis of cases where originalists dispute each other''s historical accounts (e.g., the Second Amendment debate, Commerce Clause original meaning) to measure how much indeterminacy persists in ''recovered'' historical meaning.',
    'If historical evidence is substantially indeterminate, originalism has not solved the discretion problem — it has relocated it from judge''s policy preference to judge''s interpretation of history. The constraint would be reclassified from coordination (fixing meaning) to extraction (suppression of adaptive frames under the pretense of constraint). If historical evidence is determinate, originalism genuinely limits discretion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_evidence_indeterminacy, empirical, 'Whether historical recovery is constrained or judgment-laden.').

omega_variable(
    reading_foreclosure_structure,
    'Does the originalist reading logically foreclose the living-constitutionalist reading within a single interpretive framework, or do they represent coexisting positions held by different parties?',
    'Logical analysis of core premises: if originalism asserts ''meaning is fixed at ratification'' and living-constitutionalism asserts ''meaning must evolve'', are these claims about ontological fact (one forecloses the other) or about normative methodology (both can coexist as legitimate approaches)?',
    'If the readings foreclose each other, the kernel contest is zero-sum: only one can be institutionally dominant. If they coexist, the contest is about which parties control interpretation, not about which approach is correct. Classification of the relationship affects whether the constraint is a genuine natural boundary (mountain) or an artifact of institutional power (snare/tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_structure, conceptual, 'Logical versus institutional structure of the originalist-living-constitutionalist contest.').

omega_variable(
    institutional_capture_ambiguity,
    'Is originalism primarily a methodological tool for constraining judicial discretion, or has it been captured by conservative institutional coalitions as a tool for anchoring conservative outcomes to historical rather than policy rationales?',
    'Analysis of originalist case outcomes (whether originalist methodology produces conservative outcomes at higher rate than other methodologies) and analysis of appointments (whether originalism became a credential for ideological alignment rather than neutral methodology).',
    'If captured, the constraint is primarily extractive (suppression of adaptive frames to serve conservative policy) rather than coordinative (genuine limiting principle). If genuinely neutral, extraction is an incidental side effect rather than the constraint''s function. The distinction affects whether the constraint is a snare (primary function is extraction) or a tangled rope (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_capture_ambiguity, empirical, 'Whether originalism functions as neutral limiting principle or as ideological tool.').

omega_variable(
    suppression_internalization,
    'Is the measured suppression (0.79) structural (external barriers excluding living-constitutionalist frames from law-school curricula and judicial appointments) or internalized (living-constitutionalist judges and scholars internalize originalist framing and self-suppress)?',
    'Post-constraint-removal trajectory: if living-constitutionalist interpretation resurges immediately when originalist institutional dominance relaxes (e.g., new wave of judicial appointments from non-originalist schools), suppression is primarily structural; if adaptive interpretation remains marginalized even after dominance relaxes, suppression is partially internalized.',
    'If structural, removing originalist institutional gatekeeping would rapidly restore adaptive interpretation. If internalized, the constraint''s suppression travels with the suppressed agents; removal of institutional barriers might not restore adaptive interpretive confidence or practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether suppression of adaptive interpretation is external or internalized.').

omega_variable(
    kernel_reading_relation_choice,
    'Between this originalist reading and the living-constitutionalist reading: does originalism FORECLOSE living-constitutionalism (they cannot coexist in one framework), or do they COEXIST as competing methodologies held by different parties?',
    'Can a judge hold both originalist and living-constitutionalist premises within a coherent jurisprudential framework? The historical evidence: most judges are aligned with one methodology or the other, not both; some judicial opinions invoke originalism and adaptive principles in different parts. If the premises logically contradict, the relation is foreclose; if judges can in principle hold both (but institutional constraints prevent it), the relation is coexist.',
    'Forecloses → the kernel contest is ontologically zero-sum; only one reading can be true. Coexists → the contest is institutional (which reading controls judicial authority) rather than logical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_relation_choice, conceptual, 'Logical relationship between originalist and living-constitutionalist interpretive premises.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_text__originalist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t8, us_constitution_text__originalist_reading, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(us_c_tr_t8, observed).
narrative_ontology:measurement(us_c_tr_t16, us_constitution_text__originalist_reading, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(us_c_tr_t16, observed).
narrative_ontology:measurement(us_c_tr_t24, us_constitution_text__originalist_reading, theater_ratio, 24, 0.32).
narrative_ontology:measurement_basis(us_c_tr_t24, observed).
narrative_ontology:measurement(us_c_tr_t32, us_constitution_text__originalist_reading, theater_ratio, 32, 0.37).
narrative_ontology:measurement_basis(us_c_tr_t32, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_text__originalist_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_text__originalist_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_text__originalist_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t8, us_constitution_text__originalist_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(us_c_be_t8, observed).
narrative_ontology:measurement(us_c_be_t16, us_constitution_text__originalist_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(us_c_be_t16, observed).
narrative_ontology:measurement(us_c_be_t24, us_constitution_text__originalist_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement_basis(us_c_be_t24, observed).
narrative_ontology:measurement(us_c_be_t32, us_constitution_text__originalist_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement_basis(us_c_be_t32, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_text__originalist_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_text__originalist_reading, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_text__originalist_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(us_c_su_t0, observed).
narrative_ontology:measurement(us_c_su_t8, us_constitution_text__originalist_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement_basis(us_c_su_t8, observed).
narrative_ontology:measurement(us_c_su_t16, us_constitution_text__originalist_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement_basis(us_c_su_t16, observed).
narrative_ontology:measurement(us_c_su_t24, us_constitution_text__originalist_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement_basis(us_c_su_t24, observed).
narrative_ontology:measurement(us_c_su_t32, us_constitution_text__originalist_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement_basis(us_c_su_t32, observed).
narrative_ontology:measurement(us_c_su_t40, us_constitution_text__originalist_reading, suppression_requirement, 40, 0.79).
narrative_ontology:measurement_basis(us_c_su_t40, observed).
narrative_ontology:measurement(us_c_su_t50, us_constitution_text__originalist_reading, suppression_requirement, 50, 0.79).
narrative_ontology:measurement_basis(us_c_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_text__originalist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the kernel 'us_constitution_text'. The ε-invariance principle requires separate constraints for structurally distinct readings because they produce different beneficiary/victim structures and different persistence mechanisms. The originalist reading (this file) fixes meaning at ratification, suppressing adaptive frameworks — ε high (0.68), victims: rights claimants and adaptive practitioners. The living-constitutionalist reading would permit meaning evolution, expanding access to adaptive interpretation — expected ε moderate-low, victims: formal-constraint advocates. The positivist reading would ground validity in enactment procedure independent of meaning — expected ε different again, different victim set. All three are linked via network.affects_constraints to enable contamination propagation analysis and constraint-family coherence verification. Do NOT attempt to merge the readings into one constraint with measurement-basis parameters — the three readings are three distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
