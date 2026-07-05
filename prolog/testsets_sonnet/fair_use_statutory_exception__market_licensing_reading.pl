% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fair_use_statutory_exception__market_licensing_reading, []).

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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use Fourth-Factor Market-Substitution Reading (Any Licensable Use Harms the Licensing Market)
 *   domain: legal/intellectual_property/information_economics
 *
 * SUMMARY:
 *   This story instantiates one contested reading of the fair use fourth
 *   statutory factor: the market_licensing_reading, under which any use that
 *   could in principle be licensed is treated as harming the market for
 *   licensed uses, so that fair use survives only where no market — actual or
 *   hypothetical — could ever be constructed. This reading has been advanced
 *   with increasing success as licensing intermediaries (collective licensing
 *   organizations, stock media platforms, and now AI-training data
 *   marketplaces) proliferate, because their mere existence is cited as
 *   evidence of a foreclosed market even where no actual license transaction
 *   for the specific use has occurred or would occur. This is NOT the same
 *   constraint as the narrow_defense_reading (which treats fair use as an
 *   affirmative defense narrowly construed around genuine market
 *   substitution) or the transformative_right_reading (which treats fair use
 *   as an enabling right for cultural production). The three readings produce
 *   structurally different victim sets, different epsilon values, and
 *   different doctrinal trajectories; they are linked here only through
 *   network.affects_constraints and cs_structure.reading_relations, per the
 *   kernel/reading discipline. This story's epsilon is authored as extremely
 *   high and rising, reflecting the reading's practical effect of nullifying
 *   fair use wherever licensing infrastructure exists — which, given the
 *   proliferation of clearinghouses and AI licensing marketplaces, is now
 *   nearly everywhere.
 *
 * KEY AGENTS:
 *   - rights_holder_licensing_intermediaries: primary beneficiary and agenda-setter (institutional/arbitrage) — collects fees when this reading prevails
 *   - major_content_conglomerates: primary beneficiary (institutional/arbitrage) — funds the litigation strategy
 *   - independent_researchers, documentary_filmmakers, commentary_and_criticism_publishers, archive_and_preservation_institutions: primary targets (moderate/constrained) — bear chilling effects and clearance costs
 *   - algorithmic_training_data_users: target with bifurcated power (large firms can license; small developers cannot)
 *   - federal_courts: agenda-setting observer — the seat where this reading is actively being adopted or resisted, case by case
 *   - the_public_domain_and_future_users: excluded — no standing, no voice, bears the diffuse civilizational cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.88).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.79).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.81).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use Fourth-Factor Market-Substitution Reading (Any Licensable Use Harms the Licensing Market)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "legal/intellectual_property/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'bbcf87b0-2877-4db0-aacd-c66e32e730e4').
narrative_ontology:cs_kernel_codification('bbcf87b0-2877-4db0-aacd-c66e32e730e4', fixed_text).
narrative_ontology:cs_authority_grounding('bbcf87b0-2877-4db0-aacd-c66e32e730e4', extraction).
narrative_ontology:cs_interpretation_layer_present('bbcf87b0-2877-4db0-aacd-c66e32e730e4').
narrative_ontology:cs_reading_relation('bbcf87b0-2877-4db0-aacd-c66e32e730e4', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_reading_relation('bbcf87b0-2877-4db0-aacd-c66e32e730e4', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_axiom('bbcf87b0-2877-4db0-aacd-c66e32e730e4', foundational, hypothetical_licensability_constitutes_market_harm).
narrative_ontology:cs_axiom_status(hypothetical_licensability_constitutes_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('bbcf87b0-2877-4db0-aacd-c66e32e730e4', hypothetical_licensability_constitutes_market_harm, conventional).
narrative_ontology:cs_axiom('bbcf87b0-2877-4db0-aacd-c66e32e730e4', secondary, fourth_factor_is_dispositive_over_transformativeness).
narrative_ontology:cs_axiom_status(fourth_factor_is_dispositive_over_transformativeness, holdable).
narrative_ontology:cs_axiom_grounding('bbcf87b0-2877-4db0-aacd-c66e32e730e4', fourth_factor_is_dispositive_over_transformativeness, instrumental).
narrative_ontology:cs_reference_frame('bbcf87b0-2877-4db0-aacd-c66e32e730e4', campbell_four_factor_balancing_framework).
narrative_ontology:cs_drift_state('bbcf87b0-2877-4db0-aacd-c66e32e730e4', post_licensing_marketplace_proliferation, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bbcf87b0-2877-4db0-aacd-c66e32e730e4', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rights_holder_licensing_intermediaries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, major_content_conglomerates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, collective_licensing_organizations).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, independent_researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, archive_and_preservation_institutions).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, algorithmic_training_data_users).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_primacy_doctrine).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, fourth_factor_dispositive_weight).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate licensing clearinghouses and administer permission markets for text, image, music, and now training-data use. Argue in litigation and amicus briefs that any use a licensing scheme could theoretically capture must be treated as market harm under factor four, regardless of whether a real market for that specific use currently exists. Collect fees whenever a court accepts this framing and a would-be fair user licenses instead of litigating.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rights_holder_licensing_intermediaries, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rights_holder_licensing_intermediaries, beneficiary).

% Hold large catalogs and benefit directly from a doctrine that treats hypothetical licensability as dispositive market harm. Fund the litigation strategy that established and extends this reading, and capture licensing revenue that would otherwise be unpaid fair use.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, major_content_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Administer blanket and statutory licensing schemes whose existence is cited as evidence that a market 'could' form for nearly any use. Their institutional survival depends on courts treating potential licensability as actual market foreclosure.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, collective_licensing_organizations, beneficiary,
    organized, generational, arbitrage, national).

% Quote, excerpt, and reproduce copyrighted material for scholarship. Under this reading, any excerpt a licensing platform could theoretically monetize counts against fair use, so they either pay fees they cannot afford, self-censor citations, or risk litigation they cannot sustain. Exit means abandoning the source material or the publication venue.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, independent_researchers, payer,
    moderate, biographical, constrained, national).

% Use archival footage, news clips, and music for commentary and historical work. E&O insurers and distributors now require clearance for uses that would have been fair use under a transformative-purpose test, because insurers price against the market-substitution reading's litigation risk. Distribution is effectively gated by clearance.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Publish reviews, parody, and criticism incorporating protected excerpts. The existence of licensing platforms for excerpts and quotes is cited against them even when the specific use is plainly transformative; they face demand letters and takedowns keyed to theoretical licensability rather than actual lost sales.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers, payer,
    moderate, biographical, constrained, national).

% Digitize and provide access to orphan works and out-of-commerce materials. Because rights-holder collectives now offer licensing schemes covering broad swaths of such material, courts treat the mere existence of the scheme as foreclosing fair use, even where the actual license is never sought by the true rights holder and access would otherwise be free. Cannot practically clear rights for collections at scale; exit is withdrawal of public access.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, archive_and_preservation_institutions, payer,
    moderate, civilizational, trapped, national).

% Train models on copyrighted corpora. The emergence of nascent AI-training licensing marketplaces is used, under this reading, as proof that any unlicensed training use harms a cognizable market — even for uses courts previously treated as transformative research. Large firms can negotiate bulk licenses; smaller developers cannot, and face existential litigation exposure.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, algorithmic_training_data_users, payer,
    powerful, biographical, constrained, global).

% Future readers, students, and creators who would benefit from a robust fair use doctrine and an eventual public domain have no seat in the litigation that shapes this reading; their interest in low-friction cultural reuse is not represented by any party with standing in an infringement suit.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, the_public_domain_and_future_users, excluded,
    powerless, civilizational, trapped, global).

% Adjudicate the fourth statutory factor case by case. Increasingly cite the theoretical availability of licensing (rather than an actual, functioning market) as evidence of market harm, effectively adopting the market_licensing_reading as controlling doctrine in a growing line of cases while other panels resist it.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, federal_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, federal_courts, agenda_setter).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In its narrowest defensible form, the fourth factor coordinates the boundary between compensable and uncompensable uses by asking whether a use actually displaces revenue the copyright holder would otherwise realize — preventing free-riding on functioning markets.
% TRANSFER_FUNCTION: Moves permission-clearance revenue and litigation-avoidance payments from researchers, critics, archivists, filmmakers, and smaller AI developers to rights-holder licensing intermediaries and catalog owners, by converting the mere theoretical existence of a licensing mechanism into dispositive proof of market harm.
% ABSENT_VOICES: Future users of a diminished public domain, independent creators without litigation budgets, and the broader public interest in criticism, scholarship, and preservation are not parties to the infringement suits where this reading is forged; only rights holders and defendants wealthy enough to litigate appear.
% DISAPPEARANCE_RATIONALE: If this reading of the fourth factor vanished and courts returned to asking whether an actual, functioning market exists (rather than a hypothetical one), a large volume of currently-chilled scholarship, criticism, documentary work, archival access, and smaller-scale AI training would proceed unlicensed; licensing intermediaries would lose a substantial revenue stream tied to uses no real market currently prices.
% FOUNDING_PROBLEM: The fourth statutory factor was built to prevent fair use from being invoked to substitute for an actual sale in an actual market — e.g., photocopying a textbook chapter-for-chapter instead of buying the book, when a functioning market for that exact use existed and was displaced.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars outside the licensing industry (copyright treatise authors, law review commentary, and dissenting judicial opinions) attest that the doctrine has drifted from 'does a market exist and is it displaced' to 'could a market exist,' collapsing the factor's original limiting function; rights-holder intermediaries and the conglomerates that fund clearinghouse litigation attest the current reading correctly protects licensing revenue as intended. No corroboration exists from a source independent of both litigating camps — the doctrine's operation is contested precisely along beneficiary lines.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.88, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fair_use_statutory_exception__market_licensing_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fair_use_statutory_exception__market_licensing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored extremely high (0.88 at interval end) because under this reading essentially no use escapes market-harm classification once any licensing infrastructure for that category of use exists anywhere — the doctrine's own escape valve (fair use) becomes null in practice for monetizable content, which is the overwhelming majority of it. Suppression is high (0.79) and enforced through demand letters, insurance-driven clearance requirements, and litigation risk rather than through the doctrine's text alone — the mechanism requires active assertion in each case. Theater ratio is moderate (0.42) because courts genuinely apply a four-factor test with real deliberation, but a rising share of that deliberation defends the market-substitution framing itself rather than adjudicating actual displaced sales, which is the Goodhart-style drift the measurement series traces. Accessibility collapse is high (0.81): once a party understands that any licensable use counts as market harm, the practical alternative space for unlicensed use collapses to genuinely non-monetizable, de minimis uses. Resistance is substantial (0.62) reflecting active pushback from the research, library, and open-culture communities, and from dissenting judicial opinions.
 *
 * PERSPECTIVAL GAP:
 *   From the licensing intermediary and conglomerate seats, this reading is coordination: it makes the copyright system's market signals legible and lets courts avoid case-by-case guesswork about actual market effects by treating licensability itself as the test. From the researcher, filmmaker, critic, and archivist seats, the identical structure operates as near-total extraction: a doctrine originally meant to protect a narrow class of actual market substitution has been extended to swallow the fair use exception whenever any monetization pathway can be imagined, regardless of whether it is ever used. The engine should compute divergent seat classifications from this asymmetry — agenda_setter/beneficiary seats trending toward rope-like coordination, payer seats trending toward snare-like extraction, consistent with the tangled_rope claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Rights-holder intermediaries, conglomerates, and collective licensing organizations sit near the full-beneficiary end: they collect revenue whenever the reading is applied and have arbitrage-grade exit (they choose which cases to litigate). Researchers, filmmakers, critics, and archivists sit near the full-target end: they bear the extraction through clearance fees, chilling effects, or foregone use, with constrained or trapped exit because the underlying source material is often irreplaceable. Algorithmic training-data users are differentiated by power: large firms can negotiate bulk licenses (moving them toward the beneficiary end via arbitrage), while smaller developers face the same doctrine as an existential constraint (target end, constrained exit) — this asymmetry is captured by declaring the same stakeholder group at 'powerful' with 'constrained' exit rather than splitting further, since the story's focus is the doctrinal mechanism, not firm-size stratification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing fair use from substituting for an actual, displaced sale — is contested as either still live (rights holders' position) or effectively dead as originally conceived and replaced by a much broader mandate (independent commentators' position). Classifying this as tangled_rope rather than pure snare preserves the genuine coordination kernel (courts do need SOME way to assess market effects) while naming the asymmetric extraction that has grown around it — collapsing straight to snare would erase the fact that a real coordination problem (avoiding uncompensated market substitution) motivated the factor's existence, even though its current operation under this reading functions as near-total extraction for the payer seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hypothetical_vs_actual_market_ambiguity,
    'Does the fourth factor, properly construed, require an ACTUAL functioning market for the specific use in question, or does the mere theoretical possibility of licensing suffice to establish market harm?',
    'Track circuit splits and eventual Supreme Court resolution on whether ''traditional, reasonable, or likely to be developed markets'' (per Campbell v. Acuff-Rose progeny) requires evidence of an actual market versus judicial speculation about a hypothetical one; empirical study of whether licensing platforms cited as evidence of foreclosed markets ever actually transact for the specific use categories at issue.',
    'If courts settle on requiring actual markets, this reading''s epsilon collapses toward the narrow_defense_reading''s presumably lower value; if courts settle on hypothetical sufficiency, this reading''s extraction is validated as the controlling doctrine and its epsilon is confirmed as structurally correct rather than an overreach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hypothetical_vs_actual_market_ambiguity, empirical, 'Whether market harm requires proof of an actual market or merely a hypothetical one.').

omega_variable(
    licensing_market_endogeneity,
    'Do licensing markets for previously-fair-use categories (e.g., AI training data, excerpt quotation) exist independently of the market_licensing_reading, or were they created BECAUSE courts began accepting this reading — making the ''market'' cited as justification partly an artifact of the doctrine itself?',
    'Historical analysis of licensing marketplace formation dates relative to key judicial decisions adopting the market-substitution test; interview rights-holder intermediaries about whether clearinghouse creation was a response to litigation strategy or pre-existing demand.',
    'If licensing markets are substantially endogenous to the doctrine (created to manufacture the market-harm argument), the reading is closer to a snare wearing coordination language; if markets predate and are independent of the doctrine, the coordination function is more genuine and the tangled_rope classification is better supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(licensing_market_endogeneity, conceptual, 'Whether cited licensing markets are independent evidence or circular artifacts of the doctrine.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Given that the fair use statute''s text does not specify whether the fourth factor requires actual or hypothetical market harm, is the market_licensing_reading, narrow_defense_reading, or transformative_right_reading the more textually and historically faithful reading of the kernel?',
    'This is not resolvable by this story alone — it is the subject of the sibling constraints and ongoing circuit splits; document here as the committer-axis ambiguity this story''s authoring seat had to select through.',
    'Selection of reading determines which of three structurally distinct constraints (with three different epsilon values and victim sets) is instantiated; this story deliberately instantiates only the market_licensing_reading per the ε-invariance decomposition rule.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Which kernel reading is authoritative is itself contested and not resolved within this single story.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(fair_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.24).
narrative_ontology:measurement(fair_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.29).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(fair_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(fair_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.51).
narrative_ontology:measurement(fair_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.74).
narrative_ontology:measurement(fair_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.83).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(fair_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(fair_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.56).
narrative_ontology:measurement(fair_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(fair_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.73).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked constraints decomposing the natural-language concept 'fair use fourth factor.' market_licensing_reading (this story) treats hypothetical licensability as dispositive market harm, collapsing fair use nearly to null wherever licensing infrastructure exists — extremely high epsilon, tangled_rope. narrow_defense_reading treats fair use as an affirmative defense narrowly construed around actual market substitution — moderate epsilon, likely tangled_rope or rope depending on enforcement posture. transformative_right_reading treats fair use as an enabling right where transformativeness can outweigh market harm even amid licensing schemes — lower epsilon, likely rope or scaffold. The three are not the same constraint measured three ways; each has a stable, internally-consistent epsilon and distinct beneficiary/victim structure. Network edges here document the shared kernel and the doctrinal pressure this reading exerts on its siblings (a court adopting this reading forecloses or constrains the practical availability of the transformative_right_reading in the same jurisdiction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
