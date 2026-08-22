% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__narrow_defense_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__narrow_defense_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__narrow_defense_reading
 *   human_readable: Fair Use as Narrowly Construed Affirmative Defense (Property-Primacy Reading)
 *   domain: intellectual_property_law/legal_interpretation
 *
 * SUMMARY:
 *   This story instantiates the narrow-defense reading of the fair use
 *   kernel: courts and rightsholders treat copyright as a property right
 *   first, fair use as a narrowly-construed affirmative defense the defendant
 *   must establish, and market harm (including hypothetical licensing
 *   markets) as the dispositive factor. This is NOT the transformative-right
 *   reading (which treats fair use as a facilitation mandate) or the
 *   market-licensing reading (which collapses fair use to 'no market exists')
 *   — those are separate constraints, linked here structurally. Under this
 *   reading, the four-factor test in 17 U.S.C. §107 is applied with
 *   commercial nature and market substitution given outsized weight, and
 *   courts start from a presumption against the user. The measured extraction
 *   has risen steadily since 1976 as licensing markets for excerpts, clips,
 *   and images matured — creating more 'plausible licensing markets' whose
 *   mere existence weighs against fair use, a self-reinforcing dynamic where
 *   the reading's own operation expands the category of uses it defeats.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, 0.71).
domain_priors:suppression_score(fair_use_statutory_exception__narrow_defense_reading, 0.66).
domain_priors:theater_ratio(fair_use_statutory_exception__narrow_defense_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fair_use_statutory_exception__narrow_defense_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__narrow_defense_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__narrow_defense_reading, "Fair Use as Narrowly Construed Affirmative Defense (Property-Primacy Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__narrow_defense_reading, "intellectual_property_law/legal_interpretation").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__narrow_defense_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__narrow_defense_reading, '8aa35621-fb7d-410a-8167-fa44586a4035').
narrative_ontology:cs_kernel_codification('8aa35621-fb7d-410a-8167-fa44586a4035', fixed_text).
narrative_ontology:cs_authority_grounding('8aa35621-fb7d-410a-8167-fa44586a4035', lineage).
narrative_ontology:cs_interpretation_layer_present('8aa35621-fb7d-410a-8167-fa44586a4035').
narrative_ontology:cs_reading_relation('8aa35621-fb7d-410a-8167-fa44586a4035', fair_use_statutory_exception__transformative_right_reading, coexists_with).
narrative_ontology:cs_reading_relation('8aa35621-fb7d-410a-8167-fa44586a4035', fair_use_statutory_exception__market_licensing_reading, influences).
narrative_ontology:cs_axiom('8aa35621-fb7d-410a-8167-fa44586a4035', foundational, copyright_as_property_right_primacy).
narrative_ontology:cs_axiom_status(copyright_as_property_right_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8aa35621-fb7d-410a-8167-fa44586a4035', copyright_as_property_right_primacy, conventional).
narrative_ontology:cs_axiom('8aa35621-fb7d-410a-8167-fa44586a4035', foundational, market_harm_as_dispositive_factor).
narrative_ontology:cs_axiom_status(market_harm_as_dispositive_factor, holdable).
narrative_ontology:cs_axiom_grounding('8aa35621-fb7d-410a-8167-fa44586a4035', market_harm_as_dispositive_factor, instrumental).
narrative_ontology:cs_axiom('8aa35621-fb7d-410a-8167-fa44586a4035', secondary, defendant_bears_burden_of_defense).
narrative_ontology:cs_axiom_status(defendant_bears_burden_of_defense, holdable).
narrative_ontology:cs_axiom_grounding('8aa35621-fb7d-410a-8167-fa44586a4035', defendant_bears_burden_of_defense, conventional).
narrative_ontology:cs_reference_frame('8aa35621-fb7d-410a-8167-fa44586a4035', property_primacy_common_law_baseline).
narrative_ontology:cs_drift_state('8aa35621-fb7d-410a-8167-fa44586a4035', post_campbell_v_acuff_rose_transformativeness_expansion, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8aa35621-fb7d-410a-8167-fa44586a4035', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__narrow_defense_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, major_content_licensors).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, publishing_conglomerates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, stock_footage_and_image_licensing_firms).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, independent_critics_and_commentators).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, educators_and_researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__narrow_defense_reading, parody_and_remix_artists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__narrow_defense_reading, content_platforms).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, copyright_as_property_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__narrow_defense_reading, market_harm_as_primary_fair_use_factor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate licensing divisions that sell permission for quotation, excerpting, and reuse. Litigate aggressively against uses that could plausibly have been licensed, and lobby for statutory and doctrinal readings that treat the existence of a licensing market as evidence against fair use. Collect licensing revenue directly and benefit from any judicial framing that narrows the defense's availability.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, major_content_licensors, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, major_content_licensors, agenda_setter).

% Hold large copyrighted catalogs and derive revenue from both primary sales and secondary licensing (textbook excerpts, anthology rights, clip licensing). A narrow defense reading protects this secondary revenue stream by making unauthorized uses presumptively infringing unless the defendant can overcome a heavy burden.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, publishing_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Sell licenses for discrete uses of images, footage, and music cues. Their entire business model depends on courts treating 'could this have been licensed' as dispositive; a reading that credits transformative use over market substitution would erode demand for their licenses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, stock_footage_and_image_licensing_firms, beneficiary,
    organized, biographical, mobile, global).

% Quote, excerpt, or embed copyrighted material to support critical commentary, often without funds for licensing or litigation. Under the narrow defense reading, the commercial nature of their platform (ad-supported blog, monetized video) and the mere theoretical existence of a licensing market count heavily against them, regardless of the criticism's transformative character. A single infringement letter or takedown can end the use; there is no practical way to test the defense without risking statutory damages.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, independent_critics_and_commentators, payer,
    powerless, immediate, trapped, national).

% Rely on archival footage, news clips, and cultural artifacts to construct historical arguments. Insurers and distributors require 'errors and omissions' coverage that treats fair use as too risky to rely on when a market-harm-centric reading prevails, forcing filmmakers to license material they believe is legally usable without payment — the chilling effect operates upstream of any court ever ruling.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Reproduce excerpts for classroom use, scholarship, and criticism. Under a reading where any conceivable licensing market defeats the defense, institutions increasingly require formal permissions clearance even for uses previously understood as textbook fair use, adding cost and delay and sometimes foreclosing use of unavailable-to-license material entirely.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, educators_and_researchers, payer,
    moderate, biographical, constrained, national).

% Create derivative commentary works — parody songs, remix videos, meme compilations — that are commercial by platform structure (monetized uploads) even when non-commercial in intent. The narrow reading treats platform monetization as commercial use weighing against the defense, collapsing the distinction between profiting from the original work and merely existing on a platform that shares ad revenue.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, parody_and_remix_artists, payer,
    powerless, immediate, trapped, global).

% Adjudicate fair use case by case under the four-factor test in 17 U.S.C. § 107, treating it as an affirmative defense the defendant must plead and prove. Under this reading, courts weight market-harm (factor four) and commercial-nature (factor one) heavily, and place the burden of persuasion on the alleged infringer rather than treating fair use as a user right courts must affirmatively facilitate.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, federal_courts, agenda_setter,
    institutional, generational, analytical, national).

% Operate automated content-matching and takedown systems calibrated to the narrow-defense reading's risk profile, erring toward removal when a match exists rather than adjudicating fair use, since litigation exposure and safe-harbor preservation favor over-removal. Benefit from reduced legal risk while externalizing the cost of erroneous takedowns onto uploaders.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__narrow_defense_reading, content_platforms, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__narrow_defense_reading, content_platforms, beneficiary).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__narrow_defense_reading, diffuse).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__narrow_defense_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides copyright holders and courts with a predictable, market-protective baseline for adjudicating unauthorized use: if a use could plausibly be licensed, the default runs toward infringement, which lets rightsholders build reliable licensing markets and lets courts resolve disputes without case-by-case reinvention of first principles.
% TRANSFER_FUNCTION: Moves the economic value of contested uses — quotation, excerpting, commentary, remix, archival reuse — from the party making the use to the copyright holder, by making the affirmative defense hard to establish and by placing the burden of proof and the risk of litigation on the user rather than the rights holder.
% ABSENT_VOICES: Individual users, students, small documentarians, and remix creators who cannot afford to litigate a fair use claim to a final ruling are structurally absent from the case law that defines the doctrine — the reading is built almost entirely from disputes involving parties who COULD afford to litigate, meaning the doctrine's shape reflects institutional litigants' interests disproportionately.
% DISAPPEARANCE_RATIONALE: If the narrow-defense reading were replaced overnight by a right-centered reading, licensing markets built on 'any conceivable use is licensable' would lose legal support, insurers' risk calculus for documentary and educational use would shift, platform takedown systems would need to weight transformativeness rather than mere matching, and rightsholders' secondary licensing revenue streams for excerpting and quotation would substantially contract.
% FOUNDING_PROBLEM: 17 U.S.C. § 107 was enacted to codify a pre-existing common-law safety valve permitting limited unauthorized use for criticism, comment, news reporting, teaching, scholarship, and research — a mechanism to prevent copyright from suppressing exactly the discourse it is meant to enable, without granting a blanket license to copy.
% FOUNDING_PROBLEM_CORROBORATION: Copyright treatise writers and several circuit court opinions (outside the licensing industry) attest the founding problem remains live and that the narrow-defense reading has drifted from the statute's stated purpose toward market-protection; the licensing industry itself, an interested party, attests the market-harm-centric reading is faithful to congressional intent — corroboration from outside the beneficiary set exists but is contested rather than unanimous.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__narrow_defense_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__narrow_defense_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__narrow_defense_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__narrow_defense_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__narrow_defense_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__narrow_defense_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__narrow_defense_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.71) is high because the reading routes the economic value of a wide range of socially valuable uses — criticism, education, archival documentary work, parody — to rightsholders whenever a hypothetical license could exist, regardless of whether one was ever offered or sought. Suppression (0.66) reflects the structural chilling effect: insurers, platforms, and institutions pre-emptively restrict use rather than risk litigating the defense, so the suppressive force operates upstream of any court ruling. Theater ratio is moderate-low (0.28) because the four-factor analysis is genuinely applied by courts, not merely performed — but a growing share of factor-four analysis consists of expert testimony about licensing markets that did not exist until the reading itself created demand for them, which is a real but expanding performative layer.
 *
 * PERSPECTIVAL GAP:
 *   From the rightsholder/licensor seat, the arrangement looks like a coherent, well-functioning property regime with a narrow safety valve — exactly the coordination the statute intends. From the payer seats (critics, educators, documentarians, remix artists), the same doctrinal structure operates as a standing extraction mechanism: the defense is theoretically available but practically foreclosed by cost, risk, and the ever-expanding scope of 'plausible licensing markets.' The engine should compute these as structurally different experiences of the identical rule, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Major licensors, publishers, and stock-content firms are near-full beneficiaries: they collect licensing revenue whose existence is protected precisely by this reading's market-harm emphasis, and their institutional power lets them litigate to enforce it. Independent critics, documentarians, educators, and remix artists sit near the full-target end: they bear the burden of proof, face statutory damages exposure, and have no practical exit — declining to use the material forecloses the speech or scholarship entirely, and 'trapped'/'constrained' exit options reflect that the alternative to bearing the extraction is often not making the work at all.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing copyright from suppressing criticism, scholarship, and commentary — remains live (educators and documentarians still need the safety valve), but the narrow-defense reading has drifted toward serving a different function: protecting licensing revenue streams that did not exist in 1976 and whose growth is partly endogenous to the reading itself. This is not classic mandatrophy (mandate fully dead) but a live founding problem increasingly subordinated to a market-protection function that grew up alongside it — the tangled_rope classification captures this hybrid rather than forcing a clean mountain/snare call.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_harm_endogeneity,
    'Is the growth of measured market harm under this reading a genuine reflection of harm caused by unauthorized uses, or is it partly self-generated — does treating ''could be licensed'' as dispositive create licensing markets that then retroactively justify the doctrine that created them?',
    'Longitudinal study of licensing market emergence relative to doctrinal rulings: if licensing markets for a category of use (e.g. quotation excerpts, meme clips) emerged AFTER courts began weighting hypothetical licensability, that supports endogeneity.',
    'If endogenous, the rising extraction measured here is partly an artifact of the doctrine''s own operation rather than an independent empirical fact about market harm, which would argue for discounting factor-four weight in future analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_harm_endogeneity, empirical, 'Whether licensing-market growth under this reading is caused by the doctrine or merely observed by it.').

omega_variable(
    reading_selection_as_policy_choice,
    'Is the choice among the three sibling readings (narrow-defense, transformative-right, market-licensing) a matter of correct statutory interpretation, or is it an unavoidable policy choice that the statute''s text underdetermines?',
    'Comparative analysis of §107''s legislative history and subsequent circuit splits: persistent, deep splits across decades suggest genuine textual underdetermination rather than a single correct reading awaiting judicial discovery.',
    'If underdetermined, no reading can claim to be ''the'' correct reading of the kernel, and the tangled_rope classification here reflects a policy-laden choice with real winners and losers rather than a neutral application of law — this bears directly on the mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_selection_as_policy_choice, conceptual, 'Whether the reading split reflects genuine textual indeterminacy or a resolvable interpretive question.').

omega_variable(
    commercial_nature_categorization,
    'Should platform monetization structures (ad revenue sharing on uploaded content) count as ''commercial use'' by the uploader for factor-one purposes, given the uploader often has no control over or expectation of monetization?',
    'Empirical survey of platform monetization defaults and uploader intent/awareness at time of upload, cross-referenced against how courts have actually treated platform-driven monetization in recent rulings.',
    'If platform-structural monetization is treated as equivalent to deliberate commercial exploitation, the practical scope of ''commercial use'' under this reading expands dramatically beyond what §107''s drafters likely contemplated, disproportionately affecting parody and remix artists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_nature_categorization, empirical, 'Whether platform-driven monetization should count as commercial use under factor one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__narrow_defense_reading, 1976, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t1976, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1976, 0.1).
narrative_ontology:measurement(fair_tr_t1985, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1985, 0.13).
narrative_ontology:measurement(fair_tr_t1994, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 1994, 0.16).
narrative_ontology:measurement(fair_tr_t2003, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2003, 0.2).
narrative_ontology:measurement(fair_tr_t2012, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2012, 0.24).
narrative_ontology:measurement(fair_tr_t2018, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2018, 0.26).
narrative_ontology:measurement(fair_tr_t2024, fair_use_statutory_exception__narrow_defense_reading, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(fair_be_t1976, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1976, 0.42).
narrative_ontology:measurement(fair_be_t1985, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1985, 0.48).
narrative_ontology:measurement(fair_be_t1994, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 1994, 0.54).
narrative_ontology:measurement(fair_be_t2003, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(fair_be_t2012, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2012, 0.66).
narrative_ontology:measurement(fair_be_t2018, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2018, 0.69).
narrative_ontology:measurement(fair_be_t2024, fair_use_statutory_exception__narrow_defense_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t1976, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1976, 0.35).
narrative_ontology:measurement(fair_su_t1985, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1985, 0.42).
narrative_ontology:measurement(fair_su_t1994, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 1994, 0.5).
narrative_ontology:measurement(fair_su_t2003, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2003, 0.58).
narrative_ontology:measurement(fair_su_t2012, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2012, 0.62).
narrative_ontology:measurement(fair_su_t2018, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2018, 0.64).
narrative_ontology:measurement(fair_su_t2024, fair_use_statutory_exception__narrow_defense_reading, suppression_requirement, 2024, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, transformative_right_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__narrow_defense_reading, market_licensing_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the fair_use_statutory_exception kernel. narrow_defense_reading (this file) authors high ε, treating fair use as a burden-shifted affirmative defense with market harm dispositive. transformative_right_reading authors substantially lower ε for transformative uses, treating fair use as a facilitation mandate. market_licensing_reading authors an even more mechanical market-harm test than this file, collapsing the inquiry to 'does any license market exist.' The three do not average into a single ε — each is a distinct constraint instantiated from the same statutory text and case law corpus, linked here for contamination-propagation analysis: a shift in dominant judicial reading toward transformative_right_reading would directly erode this reading's beneficiary licensing revenue base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
