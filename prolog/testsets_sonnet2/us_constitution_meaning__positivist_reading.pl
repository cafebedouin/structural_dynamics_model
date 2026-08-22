% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_meaning__positivist_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Constitutional Validity as Formal Pedigree (Positivist Reading)
 *   domain: constitutional_law/legal_theory/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the positivist reading of the contested kernel
 *   over 'what makes constitutional meaning valid': validity derives from
 *   formal enactment procedure and institutional pedigree — text ratified
 *   through Article V or the founding conventions — not from independent
 *   moral appraisal of the outcome. As a rule of recognition, it genuinely
 *   solves a coordination problem (a pluralistic society can identify 'the
 *   law' without first resolving deep moral disagreement). But the same
 *   pedigree test that provides this coordination also structurally excludes
 *   substantive justice claims that cannot be traced to a formal enactment,
 *   and it systematically favors whichever coalitions succeeded in enacting
 *   text historically, regardless of whether their preferences remain
 *   defensible. When the Article V amendment process gridlocks — which it
 *   does under current supermajority thresholds — the positivist reading
 *   offers claimants no alternative route to vindication, and courts applying
 *   it end up relying on the same historical-meaning materials an originalist
 *   would use, causing the reading to collapse toward originalism in practice
 *   precisely at the moments its independence from originalism would matter
 *   most.
 *
 * KEY AGENTS:
 *   - judicial_institution: administers and enforces the pedigree test (institutional/analytical)
 *   - enacted_majoritarian_coalitions: benefit from historical procedural wins being insulated from moral re-litigation (organized/mobile)
 *   - substantive_justice_claimants: bear the cost of exclusion from validity consideration when their claims lack textual grounding (powerless/trapped)
 *   - textually_unrecognized_minorities: pay for historical exclusion from the enactment process itself (powerless/trapped)
 *   - constitutional_gridlock_losers: experience the collapse-into-originalism dynamic when Article V is unreachable (moderate/constrained)
 *   - living_constitutionalist_advocates: excluded from the validity conversation by definitional fiat (organized/constrained)
 *   - constitutional_theorists: analytical observers of the reading's actual operation (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.52).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Constitutional Validity as Formal Pedigree (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional_law/legal_theory/political_philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '5c7901f4-a5e7-42e2-91db-0eb8bf079399').
narrative_ontology:cs_kernel_codification('5c7901f4-a5e7-42e2-91db-0eb8bf079399', fixed_text).
narrative_ontology:cs_authority_grounding('5c7901f4-a5e7-42e2-91db-0eb8bf079399', lineage).
narrative_ontology:cs_interpretation_layer_present('5c7901f4-a5e7-42e2-91db-0eb8bf079399').
narrative_ontology:cs_reading_relation('5c7901f4-a5e7-42e2-91db-0eb8bf079399', us_constitution_meaning__originalist_reading, influences).
narrative_ontology:cs_reading_relation('5c7901f4-a5e7-42e2-91db-0eb8bf079399', us_constitution_meaning__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('5c7901f4-a5e7-42e2-91db-0eb8bf079399', foundational, validity_requires_formal_pedigree_not_moral_content).
narrative_ontology:cs_axiom_status(validity_requires_formal_pedigree_not_moral_content, holdable).
narrative_ontology:cs_axiom_grounding('5c7901f4-a5e7-42e2-91db-0eb8bf079399', validity_requires_formal_pedigree_not_moral_content, conventional).
narrative_ontology:cs_axiom('5c7901f4-a5e7-42e2-91db-0eb8bf079399', foundational, moral_reasoning_excluded_from_validity_determination).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity_determination, holdable).
narrative_ontology:cs_axiom_grounding('5c7901f4-a5e7-42e2-91db-0eb8bf079399', moral_reasoning_excluded_from_validity_determination, conventional).
narrative_ontology:cs_reference_frame('5c7901f4-a5e7-42e2-91db-0eb8bf079399', formal_enactment_pedigree_test).
narrative_ontology:cs_drift_state('5c7901f4-a5e7-42e2-91db-0eb8bf079399', contemporary_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5c7901f4-a5e7-42e2-91db-0eb8bf079399', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, judicial_institution).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, enacted_majoritarian_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, procedural_legitimacy_doctrine).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, substantive_justice_claimants).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, textually_unrecognized_minorities).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, constitutional_gridlock_losers).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, rule_of_recognition_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_meaning__positivist_reading, separation_of_powers_via_pedigree).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies a rule of recognition — text, ratification history, and formal amendment procedure (Article V) — to determine whether a constitutional claim is valid law at all, prior to any assessment of whether it is just. Administers the pedigree test in every case where a claim of constitutional right is asserted, and can only change the test by reinterpreting what counts as a valid enactment.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, judicial_institution, agenda_setter,
    institutional, generational, analytical, national).

% Groups that successfully passed constitutional text or amendments through the formal Article V process (or earlier ratifying conventions) have their preferences locked in as valid law regardless of whether later generations find them just. They benefit because the positivist test asks only whether the correct procedure was followed, not whether the outcome is defensible on independent moral grounds — their historical procedural win is insulated from re-litigation on moral terms.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, enacted_majoritarian_coalitions, beneficiary,
    organized, generational, mobile, national).

% The doctrine itself is vindicated and stabilized every time a court declines to import extratextual moral principle into a validity determination — it is not an actor but a standing that gains reinforcement from each such ruling. It is not a rent-collector; it is listed for completeness as the non-agent proposition the arrangement serves.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, procedural_legitimacy_doctrine, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(us_constitution_meaning__positivist_reading, procedural_legitimacy_doctrine).

% Litigants asserting a claim to a right or protection that is morally compelling by their lights but that lacks clear textual or formally-enacted grounding — e.g. claims resting on natural-rights or evolving-morality arguments unmoored from any ratified clause. Under the positivist test their claim is not merely weighed and rejected on the merits; it is excluded from validity consideration entirely because it cannot satisfy the pedigree requirement. Exit requires either textual reinterpretation (which the reading itself forecloses) or a new Article V amendment, an option that is formally open but practically unavailable to a diffuse, unorganized claimant class.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, substantive_justice_claimants, payer,
    powerless, biographical, trapped, national).

% Groups whose interests were not represented at any ratifying convention and who lack an enacted textual hook for protection bear the cost of the pedigree requirement most acutely — the arrangement treats their absence from the historical enactment process as dispositive against them today, regardless of the injustice of that historical exclusion. They cannot retroactively acquire standing without a new formal amendment, which requires supermajorities they structurally lack.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, textually_unrecognized_minorities, payer,
    powerless, generational, trapped, national).

% Political coalitions large enough to have a plausible claim to constitutional change but unable to clear Article V's supermajority thresholds. Under strict positivism their only valid path to vindication is the amendment process itself; when that process is gridlocked, the reading offers them no alternative avenue (moral argument is excluded by design), so as a practical matter they end up relying on the same enacted-original-meaning materials an originalist would use — the reading collapses toward originalism precisely when they need it most.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_gridlock_losers, payer,
    moderate, biographical, constrained, national).

% Argue that constitutional principles endure while application should evolve with social circumstance. Under the positivist framework their interpretive method is not merely disfavored but structurally disqualified as a validity criterion — it is treated as moral reasoning masquerading as law, and is excluded from the conversation about what makes a claim valid, even though it remains a live position among judges, scholars, and litigants outside this reading's own framework.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, living_constitutionalist_advocates, excluded,
    organized, generational, constrained, national).

% Study how the positivist test performs against originalist and living-constitutionalist alternatives, tracing where the pedigree requirement produces stable law and where it produces unresolved substantive claims that gridlock cannot fix. They document the collapse-into-originalism dynamic empirically without holding a stake in any reading's success.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, constitutional_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, diffuse).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a determinate, procedure-based test for what counts as valid constitutional law, so that judges, legislators, and citizens can identify the law without needing agreement on contested moral questions — a shared rule of recognition lets a pluralistic society operate under one constitution despite deep moral disagreement.
% TRANSFER_FUNCTION: Moves the burden of legal change from moral argument to formal procedure: claims that could win on independent moral grounds are denied legal force unless they can also clear the enactment/amendment threshold, while historically-enacted preferences retain force indefinitely without needing to be re-justified morally.
% ABSENT_VOICES: Substantive justice claimants and textually unrecognized minorities are formally free to pursue Article V amendment but have no voice in day-to-day validity determinations; living constitutionalist advocates are excluded from the validity conversation by definitional fiat rather than by losing an argument on the merits.
% DISAPPEARANCE_RATIONALE: If the positivist test vanished as the operative rule of recognition, courts would need some other test for validity — likely collapsing into either originalism (fixed historical meaning) or living constitutionalism (evolving moral application) — and a wide class of claims currently barred purely on pedigree grounds would suddenly become cognizable on their substantive merits, reopening settled doctrine across many areas of constitutional law.
% FOUNDING_PROBLEM: In a pluralistic polity lacking agreement on foundational moral premises, courts needed a test for legal validity that did not require judges to adjudicate contested moral truths directly — legal positivism offered a way to identify law by its pedigree (proper enactment) rather than by its moral content, avoiding both judicial moral overreach and interminable first-order moral debate in every case.
% FOUNDING_PROBLEM_CORROBORATION: Legal positivists within the judiciary and academy (H.L.A. Hart's tradition, textualist judges) attest the problem remains live: without a pedigree test, courts would be forced into open-ended moral adjudication. Critics outside that tradition — including natural-law theorists, critical legal studies scholars, and litigants representing textually unrecognized groups — attest that the 'neutral procedure' framing itself smuggles in a substantive choice (whose historical enactments count) and that the arrangement now functions to insulate entrenched pedigree-holders from moral scrutiny rather than to solve genuine indeterminacy.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_meaning__positivist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_meaning__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than high: the pedigree test genuinely solves an indeterminacy problem and most constitutional claims ARE resolved through ordinary textual/procedural analysis without controversy — the extraction is concentrated in the subset of cases where a morally serious claim fails purely on pedigree grounds. Suppression (0.58) reflects that moral argument is not merely disfavored but categorically excluded as a validity criterion, which is a real structural bar, not just an evidentiary weight. Theater is low-moderate (0.28) and rising slowly — most invocations of the pedigree test are doing real interpretive work, but a growing share of positivist reasoning in hard cases functions as post-hoc procedural cover for outcomes reachable on other grounds, which the theater trend captures. Accessibility collapse (0.6) is substantial: once a claim is identified as lacking textual pedigree, the positivist reading treats the door as genuinely closed, not merely disfavored, and formal amendment is a de jure not de facto alternative for diffuse claimant classes. Resistance (0.55) reflects the reading's genuinely contested status among judges and theorists — it is a live, actively defended and actively opposed position, not a settled consensus.
 *
 * PERSPECTIVAL GAP:
 *   From the judicial institution's seat, the positivist test looks like principled restraint — a refusal to let unelected judges impose personal moral views as constitutional law. From the seat of a substantive justice claimant whose claim is textually unmoored, the identical test looks like a categorical refusal to even hear the merits of their claim, dressed as neutrality. The engine should compute these as structurally different experiences of the same enforced rule, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Enacted majoritarian coalitions and the judicial institution sit near the beneficiary end: the coalitions because their historical procedural victories are insulated from ongoing moral scrutiny, the institution because the pedigree test gives it a determinate, defensible basis for decision that shields it from charges of unelected moral lawmaking. Substantive justice claimants and textually unrecognized minorities sit near the full-target end: they are powerless, trapped (no realistic route to formal amendment), and the constraint's operation directly determines whether their claims are even cognizable. Constitutional gridlock losers are intermediate — moderate power, constrained exit — because they retain the formally available but practically unreachable amendment path.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (avoiding judicial moral overreach in a pluralistic polity) remains genuinely live in the abstract — societies still need some way to identify valid law without requiring moral consensus. But the founding_problem_status is authored as contested rather than simply live because a substantial body of theorists and litigants outside the beneficiary set argue the doctrine's practical function has shifted: it now serves less to avoid indeterminate moral adjudication and more to entrench historically-enacted preferences against contemporary moral challenge — the classic zombie-mandate pattern where the original coordination rationale persists as justification while the operative function has drifted toward protecting incumbent pedigree-holders. Classifying this as tangled_rope rather than snare or mountain preserves this ambiguity: the coordination function (a workable rule of recognition) is real and is not mere cover, but it is bundled with asymmetric extraction from claimants without textual pedigree, requiring active judicial enforcement to hold — exactly the tangled_rope signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    positivism_originalism_convergence_boundary,
    'When Article V is gridlocked, does the positivist reading remain analytically distinct from originalism, or does it become originalism in all but name — using the same historical-meaning materials because no other pedigree source is available?',
    'Case analysis comparing positivist and originalist reasoning specifically in gridlock-era decisions: if the two produce identical outcomes via identical materials whenever no fresh enactment is available, the readings are practically indistinguishable in exactly the cases that matter most.',
    'If the readings converge under gridlock, positivism''s claimed independence from originalism''s historical-meaning commitments is largely theoretical, and much of what distinguishes the two readings normatively (institutional-procedure grounding vs. semantic-meaning grounding) may not survive contact with hard cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(positivism_originalism_convergence_boundary, conceptual, 'Whether positivism collapses into originalism under amendment gridlock.').

omega_variable(
    pedigree_neutrality_claim,
    'Is the positivist rule of recognition genuinely neutral among moral outcomes, or does the choice of which historical enactments count as valid pedigree already embed a substantive judgment favoring the coalitions that succeeded in enacting text?',
    'Compare which groups'' preferences were structurally able to clear the historical ratification and amendment thresholds versus which groups'' preferences were structurally excluded from ever attempting enactment (e.g., disenfranchised populations at founding) — assess whether the ''neutral'' pedigree test simply ratifies who held enactment power at each historical juncture.',
    'If pedigree-neutrality is itself a substantive choice favoring historically empowered enactors, the beneficiary/victim asymmetry in this story is not incidental but constitutive of what positivism does — strengthening the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pedigree_neutrality_claim, conceptual, 'Whether the pedigree test''s apparent neutrality is itself substantively loaded.').

omega_variable(
    sibling_reading_divergence_location,
    'Where exactly does the positivist reading''s classification diverge from the originalist and living-constitutionalist readings — is it primarily in the beneficiary/victim structure, in the theater ratio, or in the founding-problem status?',
    'Cross-reference this story''s authored metrics against the sibling stories (originalist_reading, living_constitutionalist_reading) once generated, comparing extractiveness, victim sets, and founding_problem_status side by side.',
    'Establishes whether the kernel''s three readings are genuinely structurally distinct constraints (as Rule 1 requires) or whether two of them are close enough in practice to warrant re-examining the decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_divergence_location, empirical, 'Committer-frame tracking of where this reading''s structure differs from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.21).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.49).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language concept 'what makes constitutional meaning valid' per the ε-invariance principle. Each reading (positivist, originalist, living_constitutionalist) is authored as its own constraint with its own ε, beneficiary/victim structure, and claimed type, because the three readings would not share a stable ε if forced into one story. The positivist reading here is authored as tangled_rope (ε=0.52); it structurally influences the originalist reading because gridlock in the amendment process causes positivist reasoning to converge on originalist materials, and it stands in tension with the living_constitutionalist reading because that reading's central method (moral/evolving application) is precisely what the positivist rule of recognition excludes as a validity criterion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
