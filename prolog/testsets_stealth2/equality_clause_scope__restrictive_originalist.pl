% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__restrictive_originalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__restrictive_originalist, []).

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
 *   constraint_id: equality_clause_scope__restrictive_originalist
 *   human_readable: Equality Clause Scope — Restrictive Originalist Reading (Founding Contracting Class)
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This story instantiates the restrictive_originalist reading of the
 *   equality_clause_scope kernel: the founding equality commitment, on this
 *   reading, legitimately applies to the contracting class of the
 *   eighteenth-century social compact — propertied white males as political
 *   actors — and claims outside that scope acquire constitutional force only
 *   through separate amendment. The standing arrangement under assessment is
 *   that scoped arrangement itself across its life: a formula that proclaimed
 *   created equality while bounding its subjects, enforced for roughly a
 *   century through franchise qualifications, racialized citizenship law, and
 *   coverture, then displaced stepwise by the Reconstruction and suffrage
 *   amendments, surviving today as an interpretive position administered by
 *   originalist jurists. The epsilon authored here is reading-indexed: it
 *   records how this reading assesses the arrangement it defends, counting
 *   the community-defining coordination as genuine and locating the costs of
 *   exclusion outside the clause's remit — the excluded seats compute their
 *   own, sharply higher extraction from the same structure. Claim and metrics
 *   are independent: the claimed type states what the structure shows
 *   (coordination plus asymmetric extraction under active enforcement); the
 *   metrics describe the arrangement's actual operation over time. KEY AGENTS
 *   (by structural relationship): - propertied_white_male_electors: Primary
 *   beneficiary (organized/mobile) — holds franchise and full standing under
 *   the founding scope - enslaved_persons: Primary target (powerless/trapped)
 *   — wholly outside the contracting class - free_black_citizens: Target
 *   (powerless/constrained) — civic existence without the clause's protection
 *   - women_denied_franchise: Target (powerless/constrained) — governed and
 *   taxed without standing - propertyless_white_men: Conditional target
 *   (powerless/constrained) — excluded by the property qualification -
 *   founding_framers: Agenda setter (institutional/arbitrage) — authored the
 *   scoped formula - originalist_judicial_appointees: Contemporary agenda
 *   setter (institutional/arbitrage) — administers the residual scope claim -
 *   abolitionist_petitioners: Excluded voice (organized/constrained) -
 *   civil_rights_litigators: Excluded voice (organized/constrained) -
 *   constitutional_historians: Analytical observer (analytical/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__restrictive_originalist, 0.38).
domain_priors:suppression_score(equality_clause_scope__restrictive_originalist, 0.28).
domain_priors:theater_ratio(equality_clause_scope__restrictive_originalist, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, extractiveness, 0.38).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(equality_clause_scope__restrictive_originalist, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__restrictive_originalist, tangled_rope).
narrative_ontology:human_readable(equality_clause_scope__restrictive_originalist, "Equality Clause Scope — Restrictive Originalist Reading (Founding Contracting Class)").
narrative_ontology:topic_domain(equality_clause_scope__restrictive_originalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:requires_active_enforcement(equality_clause_scope__restrictive_originalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__restrictive_originalist, 'be3670bc-7c2b-47c0-b6da-12d53b9a8a7e').
narrative_ontology:cs_kernel_codification('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', fixed_text).
narrative_ontology:cs_authority_grounding('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', lineage).
narrative_ontology:cs_interpretation_layer_present('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e').
narrative_ontology:cs_reading_relation('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', equality_clause_scope__expansive_universalist, forecloses).
narrative_ontology:cs_reading_relation('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', foundational, original_public_meaning_fixes_scope).
narrative_ontology:cs_axiom_status(original_public_meaning_fixes_scope, holdable).
narrative_ontology:cs_axiom_grounding('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', original_public_meaning_fixes_scope, conventional).
narrative_ontology:cs_axiom('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', foundational, amendment_is_sole_legitimate_expansion_path).
narrative_ontology:cs_axiom_status(amendment_is_sole_legitimate_expansion_path, holdable).
narrative_ontology:cs_axiom_grounding('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', amendment_is_sole_legitimate_expansion_path, conventional).
narrative_ontology:cs_reference_frame('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', founding_contracting_class_scope).
narrative_ontology:cs_drift_state('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('be3670bc-7c2b-47c0-b6da-12d53b9a8a7e', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__restrictive_originalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__restrictive_originalist, propertied_white_male_electors).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, enslaved_persons).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, free_black_citizens).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, women_denied_franchise).
narrative_ontology:constraint_victim(equality_clause_scope__restrictive_originalist, propertyless_white_men).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, eighteenth_century_social_contract_theory).
narrative_ontology:constraint_vindicates(equality_clause_scope__restrictive_originalist, original_public_meaning_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the franchise and full civil standing under the founding framework; the scoped equality commitment guarantees their equal footing with one another and routes constitutional argument through them. Their benefit is guaranteed standing plus the presumption of being the contract's parties. Leaving would mean emigration or renouncing property and standing; almost none face pressure to go.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertied_white_male_electors, beneficiary,
    organized, generational, mobile, national).

% Are held as property inside the very framework that proclaims created equality; the narrow scope places them wholly outside the contracting class, so no equality claim runs to them under it. Manumission, flight, and maroon communities exist as routes out but are criminalized and physically lethal, and the fugitive-slave clause extends capture across state lines.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, enslaved_persons, payer,
    powerless, biographical, trapped, national).

% Live outside bondage but outside the clause's scope: barred from franchise, testimony, jury service, and equal-protection claims in most jurisdictions. Property ownership, Northern residence, and emigration schemes offer partial mobility, but every route carries legal disability and organized hostility.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, free_black_citizens, payer,
    powerless, biographical, constrained, national).

% Are taxed, governed, and economically dependent within households they cannot vote in or litigate from; coverture folds their civic existence into their husbands'. The clause's masculine subject and the original-scope doctrine place their claims outside it. Marriage, widowhood, or separate estates alter their condition but restore no independent standing.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, women_denied_franchise, payer,
    powerless, biographical, constrained, national).

% Are white and male but fail the property qualification the reading treats as constitutive of the contracting class; they pay taxes and serve in militias without voting in most states during the early republic. Accumulating land or moving west can admit them, so their exclusion is conditional rather than absolute.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, propertyless_white_men, payer,
    powerless, biographical, constrained, national).

% Drafted and ratified the framework, selecting an equality formula grand enough to proclaim created equality yet scoped so that ratification would not force the slavery question. Many personally held enslaved people or participated in the slave economy. They set the interpretive baseline this reading appeals to and could shape the rules they wrote.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, founding_framers, agenda_setter,
    institutional, generational, arbitrage, national).

% Contemporary judges and justices committed to original-public-meaning method administer what remains of the narrow-scope claim: they decide when a modern rights claim must trace to founding-era scope or to a later amendment. Their position is sustained through appointment politics and scholarly production; retirement into academia or practice is lucrative, though it means stepping off the bench where the method matters.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, originalist_judicial_appointees, agenda_setter,
    institutional, generational, arbitrage, national).

% Petitioned Congress against slavery's extension and were met with gag rules that tabled their papers without debate. Under the narrow scope their claims have no purchase on the equality commitment; they are the paradigmatic voice the scoped arrangement keeps out of the room, able to organize and speak but not to be answered within the framework.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, abolitionist_petitioners, excluded,
    organized, biographical, constrained, national).

% Bring equality claims to court on behalf of excluded groups and are told, under this reading, that the clause supplies no basis for their clients' standing — the route is amendment or ordinary legislation, not the founding commitment. They argue continuously against the scope from outside its interpretive coalition.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, civil_rights_litigators, excluded,
    organized, biographical, constrained, national).

% Reconstruct founding-era usage, ratification debates, and the scope decisions embedded in early practice. Their archival work is cited by every interpretive camp and disciplines all of them; they collect no rents from any scope outcome and can follow the evidence wherever it leads.
narrative_ontology:constraint_stakeholder(equality_clause_scope__restrictive_originalist, constitutional_historians, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equality_clause_scope__restrictive_originalist, propertied_white_male_electors).
narrative_ontology:fixing_cost_class(equality_clause_scope__restrictive_originalist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Defines the boundaries of the political community and fixes the meaning of the equality commitment: it settles who counts as a party to the social contract, gives the included class guaranteed mutual standing, and provides a determinate, supermajority-gated procedure (Article V) for changing membership.
% TRANSFER_FUNCTION: Moves political standing, franchise, and enforceable equality claims toward propertied white male actors; moves the cost of inclusion onto excluded groups, who must assemble separate supermajority coalitions — amendment by amendment — to obtain constitutional standing.
% ABSENT_VOICES: Enslaved persons, free Black citizens, women, and propertyless men — the people whose status the clause defines — had no seat at drafting or ratification. Abolitionist petitioners were gagged rather than answered; suffrage and civil rights claimants were directed out of the clause and into an amendment process they could not yet win. The apparent unanimity of the founding scope was produced by keeping these seats empty.
% DISAPPEARANCE_RATIONALE: If the narrow-scope reading vanished overnight, the scope question reopens immediately: courts and movements would fill the vacuum with one of the rival scope accounts, the interpretive coalition built around original-scope legitimacy would dissolve back into the broader originalist movement, and the settled expectation that expansion runs exclusively through Article V would lose its principal defender.
% FOUNDING_PROBLEM: Holding thirteen states with divergent economies — several built on chattel slavery — inside one ratified compact required an equality formula that could be proclaimed grandly ('created equal') while being scoped narrowly enough that it would not force the slavery question at the founding moment.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians outside the beneficiary tradition corroborate the genealogy: ratification-era correspondence, the three-fifths and fugitive-slave bargains, and the gag-rule treatment of abolitionist petitions document that scope-narrowing purchased union. Descendant-community scholarship and the Reconstruction amendment debates attest the same from the excluded side. No serious party disputes that the ratification-era problem is historical; the live dispute is over what the arrangement is for now.
narrative_ontology:disappearance_verdict(equality_clause_scope__restrictive_originalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__restrictive_originalist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__restrictive_originalist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equality_clause_scope__restrictive_originalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__restrictive_originalist, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__restrictive_originalist_tests).
:- end_tests(equality_clause_scope__restrictive_originalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.38 at interval end and reading-indexed: the referent is the standing narrow-scope arrangement as this reading itself assesses it. The reading counts the community-definition and fixed-meaning functions as genuine coordination, treats the excluded populations as outside the clause's subjects rather than as its victims, and concedes only the overhead of defensive maintenance — hence a moderate value far below what the excluded seats would author over the same referent. Suppression (0.28) is the end-state of an enforcement-decay trajectory: the coercive machinery that held the scope (disenfranchisement statutes, fugitive retrieval, coverture doctrine) was dismantled by amendment and civil-rights legislation, leaving only interpretive gatekeeping. Theater_ratio (0.42) rises steadily as the arrangement's operative function atrophied into a professionalized methodology — real archival and doctrinal work mixed with ceremonial founder-invocation — while staying below the proxy-replacement line because the reading still performs consequential work in appointment politics and judicial decision. Accessibility_collapse is moderate (0.48): understanding the originalist constraint does not close alternatives, since rival readings persist openly and the amendment path remains available. Resistance (0.60) reflects sustained scholarly, litigative, and movement opposition to the narrow scope. All three tracked series run on one shared six-point grid (1789–2026) so every metric is authored at every examined time point; the suppression series is authored deliberately (rather than left to the scalar) because the story's central dynamic is enforcement-capacity change — a ratchet upward through the crisis decades around 1820, collapse at Reconstruction, then decay.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute radically different types from the same structure. From the beneficiary seat (propertied_white_male_electors), the arrangement is near-pure coordination: guaranteed standing, predictable rules, a legitimate amendment path. From the trapped target seat (enslaved_persons), the same structure is total extraction with no exit. The intermediate targets (free_black_citizens, women_denied_franchise, propertyless_white_men) compute heavy extraction damped slightly by their constrained-but-nonzero mobility. The agenda-setter seats experience the arrangement as stewardship — the framers as authorship, the contemporary appointees as custodianship of a method that defines their professional identity. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Propertied_white_male_electors sit near the beneficiary end: the declaration in base_properties.beneficiaries maps directly to subsidized standing, amplified by their mobile exit and organized power. Enslaved_persons sit nearest the full-target end: declared victims, powerless, trapped — the exit modulation pushes them to maximal effective extraction. Free_black_citizens, women_denied_franchise, and propertyless_white_men carry high directionality with constrained exit damping it slightly below the trapped case. The agenda-setter seats derive from their administration role rather than from beneficiary/victim declarations; the framers authored the scope to secure their class's position (near-beneficiary relationship), while the contemporary appointees administer a residual claim whose returns they partly absorb as career and ideological capital. Spatial scope is national throughout: verification of scope claims spans the whole republic, which the engine reflects in its extraction scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading presents the scoped arrangement as the framework's own terms — a fixed, quasi-natural feature of the founding act rather than a maintained construct. Declaring beneficiaries, victims, and active enforcement lets the engine test that presentation against structure: a scope that must be defended by gag rules, fugitive acts, and appointment politics is maintained, not emergent. The R5 interview sharpens this: the founding problem (ratification-era coalition management that dodged the slavery question) is dead, corroborated as dead from outside the beneficiary set, yet the arrangement persists and world_rearranges if removed — the dead-problem-plus-rearrangement mismatch flags the zombie tendency, cross-checked against the rising theater series. At the same time, the classification prevents the opposite mislabeling: the amendment path and the community-definition function are real coordination goods the reading genuinely delivers to its included class, so the structure is not pure extraction. Tangled_rope is the honest structural call; the engine's per-seat computation will show how far the excluded seats' experience departs from it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (restrictive_originalist) of the equality_clause_scope kernel; would instantiating a sibling reading instead change the classification of the same standing arrangement?',
    'Compare per-seat classifications across the three sibling files over the identical referent; localize any divergence to the structural elements the readings differ on (beneficiary-set breadth, expansion-legitimacy threshold) rather than to metric noise.',
    'Under the expansive_universalist reading the victim set collapses into the beneficiary set and epsilon over the same referent rises sharply; under the progressive_textualist reading the enforcement profile shifts from interpretive gatekeeping to amendment shepherding. The classification reported here is indexical to this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this file is one of three readings of the equality_clause_scope kernel.').

omega_variable(
    scope_determination_locus,
    'Where is the disagreement among the kernel''s readings actually located — in the semantic content of the founding text, in the normative weight given to the founding act, or in the legitimacy threshold for expansion?',
    'Sibling-file comparison of declared axioms and reference frames: if the axioms differ only on the expansion threshold, the contest is procedural; if the reference frames themselves differ, the contest is semantic-historical.',
    'Determines what evidence could resolve the kernel contest: archival/historical evidence for a semantic dispute, jurisprudential argument for a normative one, or institutional design for a procedural one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_determination_locus, conceptual, 'Locating the structural element on which the sibling readings genuinely diverge.').

omega_variable(
    originalist_camp_epsilon_split,
    'Is the reading-indexed epsilon stable across this reading''s own adherents, or do originalists who treat the Reconstruction and suffrage amendments as fully authoritative assess the standing arrangement differently from scope-restorationists who treat founding-era scope as the touchstone even for interpreting amendments?',
    'Split the originalist camp analytically: compare assessments from amendment-accepting originalists against restorationist strains; if the two subgroups author materially different epsilon over the same referent, the restorationist strain warrants its own constraint file.',
    'If the camp splits, this file''s epsilon indexes only the amendment-accepting subtype, and a fourth reading joins the kernel family with its own beneficiary/victim structure and higher measured extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalist_camp_epsilon_split, empirical, 'Whether the restrictive originalist reading is internally homogeneous enough to carry a single epsilon.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__restrictive_originalist, 1776, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1789, equality_clause_scope__restrictive_originalist, theater_ratio, 1789, 0.08).
narrative_ontology:measurement(equa_tr_t1820, equality_clause_scope__restrictive_originalist, theater_ratio, 1820, 0.13).
narrative_ontology:measurement(equa_tr_t1868, equality_clause_scope__restrictive_originalist, theater_ratio, 1868, 0.26).
narrative_ontology:measurement(equa_tr_t1920, equality_clause_scope__restrictive_originalist, theater_ratio, 1920, 0.33).
narrative_ontology:measurement(equa_tr_t1964, equality_clause_scope__restrictive_originalist, theater_ratio, 1964, 0.38).
narrative_ontology:measurement(equa_tr_t2026, equality_clause_scope__restrictive_originalist, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(equa_be_t1789, equality_clause_scope__restrictive_originalist, base_extractiveness, 1789, 0.22).
narrative_ontology:measurement(equa_be_t1820, equality_clause_scope__restrictive_originalist, base_extractiveness, 1820, 0.27).
narrative_ontology:measurement(equa_be_t1868, equality_clause_scope__restrictive_originalist, base_extractiveness, 1868, 0.31).
narrative_ontology:measurement(equa_be_t1920, equality_clause_scope__restrictive_originalist, base_extractiveness, 1920, 0.34).
narrative_ontology:measurement(equa_be_t1964, equality_clause_scope__restrictive_originalist, base_extractiveness, 1964, 0.36).
narrative_ontology:measurement(equa_be_t2026, equality_clause_scope__restrictive_originalist, base_extractiveness, 2026, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1789, equality_clause_scope__restrictive_originalist, suppression_requirement, 1789, 0.7).
narrative_ontology:measurement(equa_su_t1820, equality_clause_scope__restrictive_originalist, suppression_requirement, 1820, 0.76).
narrative_ontology:measurement(equa_su_t1868, equality_clause_scope__restrictive_originalist, suppression_requirement, 1868, 0.52).
narrative_ontology:measurement(equa_su_t1920, equality_clause_scope__restrictive_originalist, suppression_requirement, 1920, 0.44).
narrative_ontology:measurement(equa_su_t1964, equality_clause_scope__restrictive_originalist, suppression_requirement, 1964, 0.35).
narrative_ontology:measurement(equa_su_t2026, equality_clause_scope__restrictive_originalist, suppression_requirement, 2026, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__restrictive_originalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__expansive_universalist).
narrative_ontology:affects_constraint(equality_clause_scope__restrictive_originalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the equality clause' decomposes into three structurally distinct readings of the equality_clause_scope kernel, each with its own epsilon, beneficiary/victim structure, and classification. This file instantiates the restrictive_originalist reading. The restrictive reading sits upstream historically — its scope-fixity premise set the baseline the other two readings define themselves against, and its high expansion threshold conditions the terrain on which the progressive_textualist reading operates. Each sibling file links back to this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
