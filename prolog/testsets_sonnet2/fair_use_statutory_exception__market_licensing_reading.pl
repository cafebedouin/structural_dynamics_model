% ============================================================================
% CONSTRAINT STORY: fair_use_statutory_exception__market_licensing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: fair_use_statutory_exception__market_licensing_reading
 *   human_readable: Fair Use as Market-Licensing Foreclosure (Fourth-Factor-Dominant Reading)
 *   domain: intellectual_property_law/information_economics
 *
 * SUMMARY:
 *   This constraint instantiates the market-licensing reading of the
 *   contested fair-use kernel: the doctrinal position that any use for which
 *   a licensing mechanism could conceivably exist necessarily harms the
 *   market for licensed uses, and that fair use survives only in the residual
 *   space where no market — actual or constructible — exists. This is not a
 *   claim about fair use in general; it is one specific reading of the
 *   statutory fourth factor that treats potential licensability as
 *   effectively dispositive, collapsing the traditional four-factor balancing
 *   test into a single-factor market test. Under this reading, licensing
 *   markets that are manufactured reactively (a rightsholder begins offering
 *   a 'clip license' specifically after litigation reveals users were relying
 *   on fair use) retroactively convert previously fair conduct into
 *   infringement, because the market's mere existence — regardless of its
 *   origin or whether it reflects genuine demand independent of the
 *   litigation — satisfies the factor. This reading has expanded steadily as
 *   licensing intermediaries have grown sophisticated at constructing markets
 *   for previously unmonetized uses specifically to win fourth-factor
 *   arguments.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fair_use_statutory_exception__market_licensing_reading, 0.89).
domain_priors:suppression_score(fair_use_statutory_exception__market_licensing_reading, 0.78).
domain_priors:theater_ratio(fair_use_statutory_exception__market_licensing_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(fair_use_statutory_exception__market_licensing_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fair_use_statutory_exception__market_licensing_reading, tangled_rope).
narrative_ontology:human_readable(fair_use_statutory_exception__market_licensing_reading, "Fair Use as Market-Licensing Foreclosure (Fourth-Factor-Dominant Reading)").
narrative_ontology:topic_domain(fair_use_statutory_exception__market_licensing_reading, "intellectual_property_law/information_economics").

domain_priors:requires_active_enforcement(fair_use_statutory_exception__market_licensing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fair_use_statutory_exception__market_licensing_reading, 'c2a5677a-730e-4872-b657-46d36e8b79a5').
narrative_ontology:cs_kernel_codification('c2a5677a-730e-4872-b657-46d36e8b79a5', fixed_text).
narrative_ontology:cs_authority_grounding('c2a5677a-730e-4872-b657-46d36e8b79a5', lineage).
narrative_ontology:cs_interpretation_layer_present('c2a5677a-730e-4872-b657-46d36e8b79a5').
narrative_ontology:cs_reading_relation('c2a5677a-730e-4872-b657-46d36e8b79a5', fair_use_statutory_exception__narrow_defense_reading, influences).
narrative_ontology:cs_reading_relation('c2a5677a-730e-4872-b657-46d36e8b79a5', fair_use_statutory_exception__transformative_right_reading, forecloses).
narrative_ontology:cs_axiom('c2a5677a-730e-4872-b657-46d36e8b79a5', foundational, potential_licensability_constitutes_market_harm).
narrative_ontology:cs_axiom_status(potential_licensability_constitutes_market_harm, holdable).
narrative_ontology:cs_axiom_grounding('c2a5677a-730e-4872-b657-46d36e8b79a5', potential_licensability_constitutes_market_harm, conventional).
narrative_ontology:cs_axiom('c2a5677a-730e-4872-b657-46d36e8b79a5', foundational, fourth_factor_is_dispositive_over_other_three).
narrative_ontology:cs_axiom_status(fourth_factor_is_dispositive_over_other_three, holdable).
narrative_ontology:cs_axiom_grounding('c2a5677a-730e-4872-b657-46d36e8b79a5', fourth_factor_is_dispositive_over_other_three, conventional).
narrative_ontology:cs_reference_frame('c2a5677a-730e-4872-b657-46d36e8b79a5', four_factor_coequal_balancing_framework).
narrative_ontology:cs_drift_state('c2a5677a-730e-4872-b657-46d36e8b79a5', contemporary_digital_licensing_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2a5677a-730e-4872-b657-46d36e8b79a5', '').
narrative_ontology:cs_kernel_id(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_intermediaries).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, major_content_conglomerates).
narrative_ontology:constraint_beneficiary(fair_use_statutory_exception__market_licensing_reading, collective_licensing_organizations).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, independent_researchers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, educators_and_students).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers).
narrative_ontology:constraint_victim(fair_use_statutory_exception__market_licensing_reading, archive_and_preservation_institutions).
narrative_ontology:constraint_vindicates(fair_use_statutory_exception__market_licensing_reading, market_harm_factor_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates licensing clearinghouses and litigates aggressively to establish that any conceivable licensing market — including markets it invents after the fact by simply offering a license — forecloses fair use under the fourth factor. Collects licensing revenue directly proportional to how narrowly fair use is construed; every case won narrowing fair use widens the paid-licensing default.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_intermediaries, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_intermediaries, beneficiary).

% Holds large catalogs and funds litigation establishing the market-harm-dominant reading as controlling precedent. Benefits whenever courts treat 'could be licensed' as equivalent to 'the use displaces a market,' since it can create a licensing scheme for nearly any use type after the fact, retroactively converting formerly fair uses into infringements.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, major_content_conglomerates, beneficiary,
    institutional, generational, arbitrage, global).

% Administers blanket licenses and collects fees; its institutional relevance depends on the market-licensing reading being controlling law, since a broad transformative-use doctrine would shrink the pool of uses requiring its licenses.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, collective_licensing_organizations, beneficiary,
    organized, generational, arbitrage, national).

% Needs to quote, excerpt, or reproduce copyrighted material for scholarship. Under this reading, the mere theoretical existence of a licensing market for excerpts — however thin, however never previously exercised — defeats the fair-use claim. Cannot afford to license every source and cannot afford to litigate a fair-use defense against a well-funded rightsholder, so self-censors.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, independent_researchers, payer,
    powerless, biographical, constrained, national).

% Uses archival footage, music clips, or news clips to document historical events. Faces 'clip licensing' markets that exist largely because rightsholders created them to close off fair-use arguments; must budget for licenses or cut footage, materially shaping what stories can be told and how.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, national).

% Copies course materials, images, or text excerpts for teaching. Publishers now sell per-use classroom licenses precisely because their existence undermines a fair-use defense under this reading — the market's mere existence, not its actual displacement, controls the analysis. Institutions purchase enterprise licenses rather than risk litigation exposure, converting a former exception into a recurring cost center.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, educators_and_students, payer,
    powerless, biographical, trapped, national).

% Publishes reviews, criticism, and parody incorporating copyrighted excerpts. Faces demand letters citing licensing markets that did not exist until the rightsholder's litigation strategy created them; must either pay, cut the excerpt, or absorb legal risk that smaller outlets cannot bear.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, commentary_and_criticism_publishers, payer,
    moderate, biographical, constrained, national).

% Digitizes and provides access to historical materials for preservation and public access. Orphan works and out-of-print materials increasingly fall under 'could theoretically be licensed' reasoning even absent any active licensor, chilling preservation projects that depend on a workable fair-use floor.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, archive_and_preservation_institutions, payer,
    moderate, civilizational, constrained, national).

% Adjudicates fair-use claims and determines how much weight the fourth statutory factor (effect on the market) carries relative to the other three. Under this reading, courts treat the mere theoretical licensability of a use as dispositive, effectively collapsing the four-factor balancing test into a single factor.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, courts_applying_fourth_factor, observer,
    institutional, generational, analytical, national).

% Would argue that transformativeness — not market substitution — should be the doctrine's organizing principle, and that a market can always be conjured for any use given enough creativity in market definition. This argument exists but is structurally sidelined whenever a court treats potential licensability as effectively dispositive on its own.
narrative_ontology:constraint_stakeholder(fair_use_statutory_exception__market_licensing_reading, transformative_use_claimants, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fair_use_statutory_exception__market_licensing_reading, rightsholder_licensing_intermediaries).
narrative_ontology:fixing_cost_class(fair_use_statutory_exception__market_licensing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a predictable, litigable standard for distinguishing infringing from non-infringing use by anchoring the analysis to an observable, quasi-objective fact: does a licensing mechanism exist or could one be readily constructed. This offers courts a tractable proxy for the four-factor test's otherwise open-ended balancing.
% TRANSFER_FUNCTION: Moves the economic value of previously-fair uses (quotation, excerpting, archival access, classroom copying, review and commentary) from users of copyrighted works to rightsholders and licensing intermediaries, converting what was a use requiring no payment into a use requiring a license fee or facing litigation exposure.
% ABSENT_VOICES: Transformative-use claimants and doctrine scholars arguing for a market-definition circularity critique (any use CAN be licensed if a rightsholder decides to offer a license, so 'could be licensed' proves nothing) are rarely centered in the litigation that establishes this reading, because the litigants who fund fourth-factor-dominant precedent are rightsholders with resources to litigate, while individual fair-use defendants often settle or fold before appellate review.
% DISAPPEARANCE_RATIONALE: If courts stopped treating potential licensability as near-dispositive, licensing markets built specifically to close off fair-use claims (classroom licensing clearinghouses, clip-licensing services for uses that were previously unlicensed) would lose much of their legal leverage, use of excerpts and quotations in scholarship, criticism, and education would expand substantially, and rightsholder revenue from these secondary licensing markets would contract.
% FOUNDING_PROBLEM: The fourth statutory factor (effect on the market for the work) was created to prevent fair use from being invoked to justify uses that genuinely substitute for a copyright holder's primary market — e.g., photocopying an entire textbook to avoid buying it — where real economic substitution was occurring.
% FOUNDING_PROBLEM_CORROBORATION: Rightsholder-side litigants and licensing organizations attest the market-substitution concern remains fully live and requires this reading to prevent erosion of licensing revenue. Independent legal scholars (outside both the rightsholder and user advocacy communities), citing empirical studies of licensing markets created reactively in response to litigation rather than organically from user demand, attest that the doctrine's original substitution concern has been supplanted by a circular test where the existence of a market is manufactured by the very party invoking it — corroboration exists from academic copyright scholarship and some appellate dissents, but no consensus resolution.
narrative_ontology:disappearance_verdict(fair_use_statutory_exception__market_licensing_reading, world_rearranges).
narrative_ontology:founding_problem_status(fair_use_statutory_exception__market_licensing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fair_use_statutory_exception__market_licensing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(fair_use_statutory_exception__market_licensing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fair_use_statutory_exception__market_licensing_reading, 0.89, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is extremely high (0.89) because the doctrine, under this reading, transfers value from a very wide class of previously-uncompensated uses (quotation, classroom copying, archival access, commentary) to rightsholders and their licensing intermediaries, and the transfer scales with the intermediaries' own capacity to construct new licensing markets — the constraint's extractive reach grows precisely as the party benefiting from it invests in expanding it. Suppression is high (0.78) and rising because enforcement increasingly relies on demand letters and strategic litigation threats rather than adjudicated market harm; most targets settle or self-censor rather than litigate a fair-use defense against a well-resourced rightsholder. Theater ratio (0.42) reflects that a meaningful share of the doctrine's operation now consists of licensing markets constructed for litigation purposes rather than organic demand — the 'market' cited as harmed is often the litigation's own byproduct. Accessibility collapse (0.72) is high because once a court accepts the market-licensing reading as controlling in a given use-type (classroom copying, clip use, excerpt quotation), the precedent forecloses that use-type across the jurisdiction, not just for the litigated parties. Resistance (0.68) is substantial: legal scholars, library associations, and educator groups actively contest this reading in briefs and law review literature, and it remains genuinely disputed rather than settled.
 *
 * DIRECTIONALITY LOGIC:
 *   Licensing intermediaries and content conglomerates sit at the full-beneficiary end: they collect licensing revenue whose scope expands directly with how aggressively courts apply the market-licensing reading, and they have the resources to litigate test cases that establish favorable precedent (arbitrage-grade exit — they choose which cases to bring). Individual researchers, students, and small commentary publishers sit at the full-target end: they are structurally powerless to contest a demand letter, have no capacity to litigate a fair-use defense to a favorable outcome, and face trapped or constrained exit (self-censor or pay). Archive institutions and documentary filmmakers occupy an intermediate position — moderate power, constrained exit — because they have some institutional capacity to negotiate but not enough to challenge the doctrine itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing genuine market substitution, e.g., photocopying an entire textbook to avoid purchase) remains partially live, which is why this reading retains genuine coordination value in some applications. But under this reading, the mandate has been extended far past cases resembling genuine substitution to cover any use for which SOME licensing scheme could be imagined — including schemes constructed after the fact by the litigant asserting harm. The classification as tangled_rope rather than pure snare reflects that a real coordination function persists (preventing actual market substitution is a legitimate concern) alongside the asymmetric extraction (the doctrine as applied under this reading reaches far beyond that function). Treating this as a pure mountain (an inevitable feature of copyright economics) would obscure that the scope of 'could be licensed' is itself actively constructed by the beneficiary class; treating it as a pure snare would understate that some fraction of the doctrine's operation addresses genuine substitution harm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_definition_circularity,
    'Is ''a licensing market exists for this use'' an objective, pre-existing fact the court discovers, or is it a fact the rightsholder can construct at will by simply announcing it will offer a license for that use type?',
    'Empirical tracing of licensing-market creation dates relative to litigation timelines: if licensing schemes for a given use-type consistently emerge only after or during litigation asserting fair use, that supports the circularity critique; if markets predate and are independent of litigation, that supports the market-licensing reading''s premise.',
    'If markets are shown to be systematically manufactured in response to litigation, the market-licensing reading''s core premise collapses into a self-fulfilling prophecy, which would support reclassifying this reading''s ε upward further (nothing is ever unlicensable if a rightsholder just decides to sell a license) or would support courts adopting the transformative_right_reading instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_definition_circularity, empirical, 'Whether the licensing markets cited as dispositive are independent facts or litigation-constructed artifacts.').

omega_variable(
    fourth_factor_weighting_ambiguity,
    'Does the statutory text and legislative history of the fourth factor support treating it as dispositive/dominant, or as one of four co-equal factors requiring genuine balancing?',
    'Doctrinal and historical analysis of the 1976 Copyright Act''s legislative history and subsequent Supreme Court fair-use jurisprudence (e.g., Campbell v. Acuff-Rose''s treatment of the four factors as interrelated rather than hierarchical) to determine whether fourth-factor dominance is a textually supported reading or a drift from the statute''s original balancing structure.',
    'If the statute and precedent support co-equal balancing, the market-licensing reading represents a doctrinal drift that inflates ε beyond what the kernel itself supports; if the fourth factor is textually or historically privileged, this reading has stronger grounding and the high ε is more clearly attributable to the kernel itself rather than to this reading''s interpretive choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fourth_factor_weighting_ambiguity, conceptual, 'Whether fourth-factor dominance is textually grounded or an interpretive drift within this reading.').

omega_variable(
    reading_selection_by_litigant_resources,
    'Does the market-licensing reading become dominant in case law because it is doctrinally superior, or because the parties who can afford to litigate fair-use cases to appellate precedent are disproportionately rightsholders who benefit from it?',
    'Comparative analysis of litigant resources and settlement rates across fair-use cases: if individual/small-institution defendants settle or fold before reaching precedent-setting appellate review at disproportionate rates relative to well-resourced rightsholder plaintiffs, that supports a selection-effect explanation for this reading''s doctrinal dominance rather than a merits-based one.',
    'A confirmed selection effect would mean the case law establishing this reading is not a neutral sampling of how courts would rule on the merits across the full population of fair-use disputes, but a biased sample skewed toward rightsholder-favorable outcomes — relevant to how much weight this reading''s precedential dominance should carry as evidence of the kernel''s ''true'' content.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_by_litigant_resources, empirical, 'Whether the reading''s case-law dominance reflects litigant resource asymmetry rather than doctrinal merit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fair_use_statutory_exception__market_licensing_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fair_tr_t0, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fair_tr_t8, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 8, 0.25).
narrative_ontology:measurement(fair_tr_t16, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 16, 0.3).
narrative_ontology:measurement(fair_tr_t24, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 24, 0.35).
narrative_ontology:measurement(fair_tr_t32, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 32, 0.39).
narrative_ontology:measurement(fair_tr_t40, fair_use_statutory_exception__market_licensing_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(fair_be_t0, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(fair_be_t8, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(fair_be_t16, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(fair_be_t24, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 24, 0.78).
narrative_ontology:measurement(fair_be_t32, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 32, 0.85).
narrative_ontology:measurement(fair_be_t40, fair_use_statutory_exception__market_licensing_reading, base_extractiveness, 40, 0.89).

% Suppression requirement over time
narrative_ontology:measurement(fair_su_t0, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fair_su_t8, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 8, 0.55).
narrative_ontology:measurement(fair_su_t16, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(fair_su_t24, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(fair_su_t32, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(fair_su_t40, fair_use_statutory_exception__market_licensing_reading, suppression_requirement, 40, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fair_use_statutory_exception__market_licensing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(fair_use_statutory_exception__market_licensing_reading, 0.1).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__narrow_defense_reading).
narrative_ontology:affects_constraint(fair_use_statutory_exception__market_licensing_reading, fair_use_statutory_exception__transformative_right_reading).

% DUAL FORMULATION NOTE:
% Part of a three-member constraint family decomposing the colloquial 'fair use doctrine' kernel (fair_use_statutory_exception) into structurally distinct readings per the ε-invariance principle: market_licensing_reading (this file, ε=0.89, tangled_rope — fourth-factor-dominant, doctrine collapses to near-null in practice), narrow_defense_reading (property-centric affirmative-defense framing, expected intermediate ε), and transformative_right_reading (transformativeness-centric framing enabling cultural production, expected low ε). All three readings are linked bidirectionally via affects_constraints because case-law outcomes under one reading create precedential and market pressure affecting the others' operating environment — e.g., a market constructed to defeat a claim under this reading becomes evidence cited (or contested) under the sibling readings' analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
