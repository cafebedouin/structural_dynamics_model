% ============================================================================
% CONSTRAINT STORY: us_constitution_meaning__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: us_constitution_meaning__positivist_reading
 *   human_readable: Procedural-Pedigree Validity Discipline (Positivist Reading of Constitutional Meaning)
 *   domain: constitutional law/legal theory/political philosophy
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   'constitutional meaning': the positivist claim that constitutional
 *   validity derives from formal enactment procedures and institutional
 *   authority rather than external moral principles. The standing arrangement
 *   under contest is American constitutional practice disciplined by that
 *   claim — courts trace validity to ratification and Article V enactment
 *   chains, moral premises are barred from the validity determination, and
 *   formal amendment is the sole legitimate channel of textual change. The
 *   epsilon referent is that standing arrangement, assessed by this reading's
 *   own lights: the positivist tradition itself concedes (Hart explicitly)
 *   that source-based validity can entrench grave injustice, so the reading's
 *   own account registers real costs borne by justice claims that lack
 *   textual support, while treating the coordination function — a shared,
 *   determinate criterion of legality that ends the regress of 'who decides
 *   what counts as valid law' — as genuine and primary. Per naming rules, the
 *   delta's named beneficiary 'procedural legitimacy' is a proposition and is
 *   routed to vindicated_propositions (propositions collect no rents); the
 *   real actors who collect under the arrangement appear under beneficiaries.
 *   Claimed type and metrics are independent authored facts: I claim
 *   tangled_rope because I believe the structure pairs a genuine coordination
 *   function with asymmetric extraction requiring active enforcement; the
 *   metrics are my descriptive estimates of how the arrangement actually
 *   operates.
 *
 * KEY AGENTS:
 *   - - federal_judiciary_institution: Primary agenda-setter and collector (institutional/identity_locked) — administers the validity determination and receives its authority shield
 *   - - article_v_amendment_bodies: Gatekeeping agenda-setter (institutional/constrained) — holds exclusive control of formal constitutional change
 *   - - organized_legal_profession: Beneficiary (organized/mobile) — converts mastery of the closed source-system into credentialing and market position
 *   - - general_public_under_enacted_law: Diffuse beneficiary with secondary payer position (moderate/constrained)
 *   - - unenumerated_rights_movements: Payer (organized/constrained) — presses claims the validity rule bars from the deciding forum
 *   - - territory_residents_without_full_representation: Payer (powerless/trapped) — bears procedurally settled exclusion with no voting exit
 *   - - natural_law_jurisprudence_tradition: Excluded voice (moderate/mobile) — its premises are precisely what the validity rule bars
 *   - - comparative_jurists_analytical: Analytical observer (analytical/analytical) — sees the full structure across systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_meaning__positivist_reading, 0.66).
domain_priors:suppression_score(us_constitution_meaning__positivist_reading, 0.7).
domain_priors:theater_ratio(us_constitution_meaning__positivist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(us_constitution_meaning__positivist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_meaning__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_meaning__positivist_reading, "Procedural-Pedigree Validity Discipline (Positivist Reading of Constitutional Meaning)").
narrative_ontology:topic_domain(us_constitution_meaning__positivist_reading, "constitutional law/legal theory/political philosophy").

domain_priors:requires_active_enforcement(us_constitution_meaning__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_meaning__positivist_reading, '17229617-2305-4a25-9cfb-943e61e61c40').
narrative_ontology:cs_kernel_codification('17229617-2305-4a25-9cfb-943e61e61c40', fixed_text).
narrative_ontology:cs_authority_grounding('17229617-2305-4a25-9cfb-943e61e61c40', lineage).
narrative_ontology:cs_interpretation_layer_present('17229617-2305-4a25-9cfb-943e61e61c40').
narrative_ontology:cs_reading_relation('17229617-2305-4a25-9cfb-943e61e61c40', us_constitution_meaning__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('17229617-2305-4a25-9cfb-943e61e61c40', us_constitution_meaning__living_constitutionalist_reading, influences).
narrative_ontology:cs_axiom('17229617-2305-4a25-9cfb-943e61e61c40', foundational, validity_requires_enactment_pedigree_only).
narrative_ontology:cs_axiom_status(validity_requires_enactment_pedigree_only, holdable).
narrative_ontology:cs_axiom_grounding('17229617-2305-4a25-9cfb-943e61e61c40', validity_requires_enactment_pedigree_only, conventional).
narrative_ontology:cs_axiom('17229617-2305-4a25-9cfb-943e61e61c40', secondary, moral_reasoning_excluded_from_validity_determination).
narrative_ontology:cs_axiom_status(moral_reasoning_excluded_from_validity_determination, holdable).
narrative_ontology:cs_axiom_grounding('17229617-2305-4a25-9cfb-943e61e61c40', moral_reasoning_excluded_from_validity_determination, instrumental).
narrative_ontology:cs_reference_frame('17229617-2305-4a25-9cfb-943e61e61c40', enactment_pedigree_validity).
narrative_ontology:cs_drift_state('17229617-2305-4a25-9cfb-943e61e61c40', contemporary_amendment_gridlock_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17229617-2305-4a25-9cfb-943e61e61c40', '').
narrative_ontology:cs_kernel_id(us_constitution_meaning__positivist_reading, us_constitution_meaning).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, federal_judiciary_institution).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, article_v_amendment_bodies).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, organized_legal_profession).
narrative_ontology:constraint_beneficiary(us_constitution_meaning__positivist_reading, general_public_under_enacted_law).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, unenumerated_rights_movements).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, territory_residents_without_full_representation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_meaning__positivist_reading, general_public_under_enacted_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines what counts as valid constitutional authority case by case, tracing enactment chains and refusing moral premises not anchored in enacted text. Each refusal reinforces the automatic legitimacy its decisions enjoy — the institution never has to defend results on contested moral ground. Abandoning the procedural stance would strip the institution of its authority rationale, so the stance is self-perpetuating; the institution has effectively become the function it performs.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, federal_judiciary_institution, agenda_setter,
    institutional, generational, identity_locked, national).

% Congress and the state legislatures exclusively control formal constitutional change through the two-stage supermajority process. They decide which moral and political claims can ever acquire textual status. The channel is exercised rarely and at enormous organizational cost, so their gatekeeping position persists mostly unexercised; they cannot loosen the supermajority thresholds without first clearing those same thresholds.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, article_v_amendment_bodies, agenda_setter,
    institutional, generational, constrained, national).

% Law schools, bar associations, and practitioners convert mastery of the closed source-and-precedent system into credentialing power, curricular authority, and market position. Members can move between practice areas and jurisdictions, and some publicly dissent from procedural orthodoxy while keeping professional standing — exit is comparatively cheap for this seat.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, organized_legal_profession, beneficiary,
    organized, biographical, mobile, national).

% Receives predictable, uniform adjudication under publicly known rules and shares in the governmental stability the arrangement purchases. Absorbs the accumulating costs wherever procedurally settled arrangements entrench conditions the public would not have chosen. Individual exit means emigration, which is costly; collective influence runs through the same gated amendment channel.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, general_public_under_enacted_law, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_meaning__positivist_reading, general_public_under_enacted_law, payer).

% Coalitions pressing claims — equal-status guarantees, voting representation, new protections — whose proposals have repeatedly failed to clear enactment. Under the validity rule their arguments carry no weight in the deciding forum however compelling on their merits; their remaining routes lead back through the same gate their opponents control, or through persuading courts the rule directs away from their premises. Abandoning the claims would dissolve the movements' organizing purpose.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, unenumerated_rights_movements, payer,
    organized, generational, constrained, national).

% Residents of the capital district and the territories pay federal taxes and serve in the armed forces while electing no voting members of Congress. Their status is fixed by statutes and organic acts only Congress can revise, and the formal equality remedy would require either an act of the gatekeeper or a constitutional change the channel rarely delivers. Relocation to a state is the principal individual exit and carries heavy personal and economic cost.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, territory_residents_without_full_representation, payer,
    powerless, generational, trapped, national).

% Scholars and jurists arguing that validity severed from moral adequacy licenses entrenched injustice. They publish, teach, and litigate at the margins of the system, but their core premise is precisely what the validity rule bars from the deciding forum — they are present in the society and the academy yet absent from the conversation that determines validity.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, natural_law_jurisprudence_tradition, excluded,
    moderate, generational, mobile, national).

% Legal theorists comparing validity doctrines across constitutional systems. They trace how the pedigree rule distributes authority, which claims it silences, and how rival systems resolve the same regress problem differently. They hold no stake in the arrangement's continuation or demise.
narrative_ontology:constraint_stakeholder(us_constitution_meaning__positivist_reading, comparative_jurists_analytical, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_meaning__positivist_reading, federal_judiciary_institution).
narrative_ontology:fixing_cost_class(us_constitution_meaning__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, determinate criterion for what counts as valid constitutional authority, ending the regress of 'who decides what law is' by fixing validity in enactment pedigree rather than contested moral truth — officials, courts, and citizens coordinate on source rather than on philosophy.
% TRANSFER_FUNCTION: Moves interpretive and legitimating authority from moral reasoners and citizens pressing unenacted claims to holders of institutional office (courts, amendment majorities, the credentialed profession); correspondingly moves the cost of unresolved injustice onto those whose claims lack textual support.
% ABSENT_VOICES: Natural-law jurists and the unenacted-claim movements are socially and academically present but structurally absent from the validity-determining forum: they would argue that validity without moral adequacy licenses entrenched injustice, and that the unanimity of procedural legitimacy rests on never seating the people whose claims the procedure defeats.
% DISAPPEARANCE_RATIONALE: If the pedigree-validity rule vanished overnight, every official act would lose its ready-made legitimacy certificate; courts, Congress, and the states would need a replacement criterion — historical meaning, evolving morality, popular consent — and the entire legal order would reorganize around whichever criterion won the resulting contest. Nothing about current arrangements survives the loss unchanged.
% FOUNDING_PROBLEM: Securing a stable, non-arbitrary criterion of constitutional authority for a large republic under a written frame — a criterion that did not depend on permanently resolving moral disagreement, and that could consolidate union supremacy and predictable government after ratification.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting seats: jurisprudential scholarship across rival traditions (including the positivist reading's fiercest critics, who concede the determinacy problem while disputing this solution to it), comparative constitutional practice (numerous systems independently adopt source-based validity criteria), and the routine operational materials of governance — judicial oaths, legislative procedure manuals, bar examination content — that run on pedigree without invoking moral theory.
narrative_ontology:disappearance_verdict(us_constitution_meaning__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_meaning__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_meaning__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_meaning__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_meaning__positivist_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.66 at interval end): substantial but bounded. The arrangement moves interpretive and legitimating authority to office-holders and imposes the full cost of unresolved injustice on those whose claims lack textual support, yet it also delivers a real product — determinate, uniform adjudication — that the payer seats partly consume. The rising trajectory tracks the atrophy of the amendment channel (one amendment ratified since 1971, and that one pending for 202 years): as formal change becomes practically unavailable, the frozen text hardens and the arrangement's costs concentrate on the excluded. Suppression (0.70): the exclusion of moral reasoning is not self-maintaining; it requires courts to reject morally compelling arguments case by case, bar associations and law schools to police professional orthodoxy, and the academy to treat natural-law premises as outside the law school curriculum's center. Suppression is authored as a raw structural property — the engine scales only extractiveness. Theater ratio (0.34, rising): the procedural apparatus is genuinely functional, but a growing share of activity is performative — 'we merely apply enacted text' rhetoric that diverges from visibly discretionary construction, bicentennial pageantry substituting for amendment, and solemn citation of a change channel almost never used. Accessibility collapse (0.45): alternatives remain genuinely accessible — the two sibling readings are live positions, natural-law jurisprudence persists, and several jurisdictions and international systems ground validity differently — so the constraint closes far fewer doors than a natural limit would. Resistance (0.55): sustained doctrinal resistance from rival readings, recurring political movements demanding textual change the channel cannot deliver, and periodic court-curbing pressure when procedural outcomes offend substantive majorities. The temporal series run on ONE shared grid (t = 0,10,20,30,40,50,60) so every metric is authored at every examined point; the suppression series is authored because the story specifically traces the hardening of enforcement capacity against moral-reasoning incursion, not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by design. From the federal_judiciary_institution seat, the arrangement is protective: a determinate validity criterion shields every decision from the charge of personal moral preference, and the institution's authority depends on maintaining it. From the unenumerated_rights_movements and territory_residents seats, the identical structure operates as a barred door: their claims are invalid by definition regardless of merit, and the only legitimate remedy runs through a gate their opponents control. From the organized_legal_profession seat it is an asset; from the general-public seat it is a mixed purchase of predictability at the price of entrenched conditions. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits near the beneficiary end (collects the authority yield of every validity determination; identity-lock means it cannot exit without dissolving its own legitimacy rationale). The amendment bodies are beneficiary-gatekeepers whose constrained exit reflects that they are bound by the very supermajority thresholds they administer. The legal profession is strongly subsidized (credential rents from the closed source system, mobile exit). The general public sits near symmetric: real coordination benefit, diffuse absorption of entrenched-injustice costs. The payer movements sit near the target end (their claims are definitionally invalid; constrained exit), and territory residents sit nearest the full-target end (trapped — no voting representation and relocation as the only exit). The natural-law tradition is excluded rather than coordinated: its exclusion is what the enforcement machinery maintains, so it registers as suppressed voice rather than as a directionality seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — securing a stable criterion of constitutional authority that did not require resolving permanent moral disagreement — remains live: every rival tradition concedes the determinacy problem even while disputing this solution, and no seat proposes abolishing validity criteria altogether. The arrangement is therefore not mandate-outlived, and the status-times-verdict consumer reads live paired with world_rearranges, producing no zombie flag. Mandatrophy analysis guards in BOTH directions here: against mislabeling the arrangement as pure extraction (which would erase the genuine coordination function that ends the legal-regress problem and delivers uniform adjudication), and against mislabeling it as pure coordination (which would erase the documented concentration of costs on seats structurally unable to reach the remedy channel). The forward risk vector is drift: if theater continues rising while the amendment channel stays moribund, the arrangement migrates toward snare flavor (coordination story intact, delivery hollowed) or toward the piton cell if the profession stops profiting enough to maintain it. The measurements are designed to make that migration visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_positivist,
    'This constraint is one reading (positivist_reading) of kernel us_constitution_meaning; what structurally changes under the sibling readings, and where exactly is the disagreement located?',
    'Compare against the authored stories us_constitution_meaning__originalist_reading and us_constitution_meaning__living_constitutionalist_reading: the disagreement sits at the validity-conferring element (enactment pedigree versus ratification-fixed meaning versus evolving application), which shifts each reading''s victim set and enforcement profile.',
    'Under the living sibling, unenacted justice claims gain a legitimate channel and this story''s payer seats shrink or dissolve; under the originalist sibling, contested interpretations harden into fixed content and the payer set expands to everyone disadvantaged by frozen meaning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_positivist, conceptual, 'Committer-frame routing: this story''s classification is indexed to one reading of a three-way kernel contest.').

omega_variable(
    amendment_gridlock_collapse_into_originalism,
    'Does the positivist reading collapse into originalism in practice now that the amendment channel is effectively gridlocked — i.e., when formal change is unavailable, does validity-from-enacted-text become indistinguishable in operation from meaning-frozen-at-ratification?',
    'Measure amendment frequency against the rate of effective constitutional change routed through judicial construction: if nearly all operative change occurs by interpreting a frozen text, the practical difference between the readings approaches zero and the positivist reading functions as originalism''s legitimacy cover.',
    'If collapsed, this constraint''s extraction profile converges with the originalist sibling''s, the declared coordination function narrows to preserving the status quo, and the tangled_rope reading trends snare-flavored over the measured interval.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_gridlock_collapse_into_originalism, empirical, 'Whether amendment-gridlock collapses the positivist/originalist distinction in practice.').

omega_variable(
    victim_status_of_unenacted_claims,
    'Are holders of substantively compelling but textually unsupported justice claims genuine victims of the validity rule, or merely participants in a fair procedure who have not yet won enactment?',
    'Apply an asymmetry test: examine whether the arrangement blocks channels available to similarly positioned claims that already possess textual support — if the only difference between winner and loser is prior enactment success, the losers are fair-procedure participants; if the rule additionally forecloses routes (judicial recognition, state-level parallelism) that winners never needed, the losses are structural.',
    'If fair-procedure-participant, measured extractiveness drops materially and the classification trends toward rope; if structurally barred, the tangled_rope claim is confirmed and the payer declarations stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_status_of_unenacted_claims, conceptual, 'Conceptual ambiguity in the victim declaration: procedural losers versus structurally excluded claimants.').

omega_variable(
    internalized_separation_thesis,
    'Is the exclusion of moral reasoning from validity determination maintained by external enforcement (court rejections, bar disciplinary and curricular norms) or by internalized professional identity (''law is not morality'') that would persist if enforcement slackened?',
    'Track doctrinal and curricular outcomes where external enforcement weakens — jurisdictions adopting moral-reasoning-friendly validity standards, law schools diversifying methods training — and test whether graduates trained under the old regime revert once sanctions lift.',
    'If substantially internalized, effective suppression exceeds the structural measure and would survive formal liberalization; the constraint would persist through the profession''s self-conception even after institutional rules changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_separation_thesis, empirical, 'Structural versus internalized maintenance of the law/morality separation in professional formation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_meaning__positivist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_meaning__positivist_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_meaning__positivist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_meaning__positivist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_meaning__positivist_reading, theater_ratio, 30, 0.25).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_meaning__positivist_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_meaning__positivist_reading, theater_ratio, 50, 0.31).
narrative_ontology:measurement(us_c_tr_t60, us_constitution_meaning__positivist_reading, theater_ratio, 60, 0.34).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_meaning__positivist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(us_c_be_t10, us_constitution_meaning__positivist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(us_c_be_t20, us_constitution_meaning__positivist_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(us_c_be_t30, us_constitution_meaning__positivist_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(us_c_be_t40, us_constitution_meaning__positivist_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(us_c_be_t50, us_constitution_meaning__positivist_reading, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(us_c_be_t60, us_constitution_meaning__positivist_reading, base_extractiveness, 60, 0.66).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t0, us_constitution_meaning__positivist_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(us_c_su_t10, us_constitution_meaning__positivist_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(us_c_su_t20, us_constitution_meaning__positivist_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(us_c_su_t30, us_constitution_meaning__positivist_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(us_c_su_t40, us_constitution_meaning__positivist_reading, suppression_requirement, 40, 0.65).
narrative_ontology:measurement(us_c_su_t50, us_constitution_meaning__positivist_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(us_c_su_t60, us_constitution_meaning__positivist_reading, suppression_requirement, 60, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_meaning__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_meaning__positivist_reading, us_constitution_meaning__living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'constitutional validity.' The natural-language concept covers three structurally distinct claims with different epsilon values, victim sets, and enforcement profiles: this file instantiates the positivist reading (validity from enactment pedigree; victims are claims lacking textual support); us_constitution_meaning__originalist_reading instantiates semantic fixation at ratification; us_constitution_meaning__living_constitutionalist_reading instantiates evolving application. The positivist reading upstream-influences the living reading by narrowing the legitimate channel through which evolution may occur (institutional interpretation rather than attitudinal shift), and coexists with the originalist reading, which frequently borrows positivist validity while adding a semantic-freeze premise. Each member links the others via affects_constraints per the family rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
