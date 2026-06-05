% ============================================================================
% CONSTRAINT STORY: monopoly_transfer_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_monopoly_transfer_mechanism, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: monopoly_transfer_mechanism
 *   human_readable: Monopoly Transfer Mechanism: Copyright as Contested Kernel (Statute of Anne Reading)
 *   domain: legal_history/intellectual_property/commitment_system
 *
 * SUMMARY:
 *   The Statute of Anne (1710) marks the founding moment of copyright law in
 *   the English-speaking world, establishing the legal principle that
 *   intellectual property can be owned, transferred, and monopolized. The
 *   constraint examined here is not copyright itself but the *transfer
 *   mechanism* — the structural act by which monopoly power was transferred
 *   from the Crown-sanctioned Stationers' Company to the abstract class of
 *   'authors' via statutory language. This transfer is contested at the
 *   kernel level: does the statute represent the moment when author-copyright
 *   *became thinkable* as a conceptual category (synchronic reading), or does
 *   it represent the moment when the Stationers' monopoly was first
 *   *institutionally held* under new legitimacy (diachronic reading)? The
 *   difference is not merely philosophical — it determines whether copyright
 *   is a reform that genuinely redistributed power or a consolidation that
 *   relabeled and preserved extraction under a new ethical frame. The
 *   constraint exhibits tangled rope characteristics: genuine coordination
 *   function (authors gain incentive to publish, enabling knowledge
 *   distribution) exists alongside persistent extraction (the reading public
 *   is excluded by monopoly pricing; derivative authors cannot build on
 *   published works without permission). Theater ratio increases across the
 *   interval from 0.35 (1660, naked monopoly) to 0.58 (1710, monopoly wrapped
 *   in 'author rights' legitimacy), reflecting the increasing performative
 *   content of the regime as its original economic justification degrades.
 *   Extractiveness decreases slightly from 0.68 to 0.52 across the interval
 *   because the statute includes genuine limitations (author reversion
 *   rights, term limits, registration requirements) on the pure Stationers'
 *   Company extraction, but the constraint remains high-extraction overall.
 *
 * KEY AGENTS:
 *   - Stationers' Guild: Primary beneficiary (institutional/arbitrage) — maintains economic monopoly control over text distribution; benefits from statutory legitimation of monopoly via 'author rights' framing
 *   - Crown Licenser: Secondary beneficiary (institutional/arbitrage) — cedes direct censorship control to stationers in exchange for political leverage; copyright monopoly enables indirect information control
 *   - Reading Public: Primary victim (powerless/trapped) — excluded from legal access to texts outside monopoly control; trapped by criminalization of piracy as alternative distribution
 *   - Derivative Authors: Secondary victim (powerless/trapped) — prevented from legally building on, translating, or adapting published works; requires copyright holder permission for derivative works
 *   - Original Authors: Ambiguous agent (moderate/constrained) — nominally become beneficiaries under statute but remain contractually subordinate to stationers; experience mixed coordination (new incentive to publish) and extraction (stationers control terms, monopoly profits)
 *   - Synchronic Observer: Analytical position (analytical/analytical) — emphasizes statute's conceptual innovation and author-rights emergence; sees genuine coordination function
 *   - Diachronic Observer: Analytical position (analytical/analytical) — emphasizes statute's consolidation of stationer control; sees extraction mechanism persistence beneath conceptual reframing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(monopoly_transfer_mechanism, 0.52).
domain_priors:suppression_score(monopoly_transfer_mechanism, 0.65).
domain_priors:theater_ratio(monopoly_transfer_mechanism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(monopoly_transfer_mechanism, extractiveness, 0.52).
narrative_ontology:constraint_metric(monopoly_transfer_mechanism, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(monopoly_transfer_mechanism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(monopoly_transfer_mechanism, tangled_rope).
narrative_ontology:human_readable(monopoly_transfer_mechanism, "Monopoly Transfer Mechanism: Copyright as Contested Kernel (Statute of Anne Reading)").
narrative_ontology:topic_domain(monopoly_transfer_mechanism, "legal_history/intellectual_property/commitment_system").

domain_priors:requires_active_enforcement(monopoly_transfer_mechanism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(monopoly_transfer_mechanism, '231c2d40-f196-4560-897f-7d86306cef4f').
narrative_ontology:cs_created_at('231c2d40-f196-4560-897f-7d86306cef4f', '').
narrative_ontology:cs_kernel_codification('231c2d40-f196-4560-897f-7d86306cef4f', fixed_text).
narrative_ontology:cs_authority_grounding('231c2d40-f196-4560-897f-7d86306cef4f', lineage).
narrative_ontology:cs_interpretation_layer_present('231c2d40-f196-4560-897f-7d86306cef4f').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(monopoly_transfer_mechanism, stationers_guild).
narrative_ontology:constraint_beneficiary(monopoly_transfer_mechanism, crown_licenser).
narrative_ontology:constraint_victim(monopoly_transfer_mechanism, reading_public).
narrative_ontology:constraint_victim(monopoly_transfer_mechanism, derivative_authors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: READING PUBLIC (SNARE) — Trapped in a monopoly structure that criminalizes unauthorized copying. No exit option; cannot legally access texts outside the Stationers' approved list. Bears full extraction cost through inflated prices and restricted availability. The monopoly persists through legal suppression of alternatives (piracy is criminalized).
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DERIVATIVE AUTHORS (SNARE) — Trapped by copyright enforcement that prevents building on existing works without Stationer permission. Cannot legally create adaptations, translations, or commentaries on published texts. Structural extraction: monopoly holder controls derivative works, preventing follow-on creation.
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: STATIONERS' GUILD (ROPE) — Primary beneficiary experiencing copyright as pure coordination of text distribution. The guild benefits from monopoly control but frames it as legitimate economic incentive: 'We fund the expensive work of printing and binding; copyright ensures we recover costs.' This is genuine coordination from their position — the constraint solves their capital-recovery problem.
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CROWN LICENSER (ROPE) — Crown grants monopoly to stationers in exchange for control of the printing industry: censorship mechanism, political loyalty, and revenue. From the crown's perspective, copyright is coordination — it centralizes control of information flow. Both the crown and stationers see the constraint as solving their respective problems (economic incentives for stationers; political control for crown).
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SYNCHRONIC READING — AT THE MOMENT OF STATUTE (1710) (TANGLED ROPE) — Reads the Statute of Anne as a conceptual innovation: the *idea* that authors (not just printers/stationers) might hold property rights in texts became thinkable for the first time. The statute transfers monopoly from stationers to authors (nominally), creating an ethical and legal conceptual space where authors are recognized as originators deserving reward. This reading emphasizes the coordination function: 'We now have a framework where authors have incentive to create.' The tangled-rope classification reflects that the statute includes genuine coordination (author incentive framework) alongside extraction (the monopoly still excludes the public and prevents derivative works).
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: DIACHRONIC READING — INSTITUTIONAL OCCUPATION (TANGLED ROPE) — Reads the Statute of Anne as the moment when the monopoly was *first institutionally held* by stationers under the legal guise of 'author protection.' The diachronic reading asks: did the statute genuinely transfer power to authors, or did it merely relabel and legitimize the existing stationer monopoly? The extraction mechanism remains constant (monopoly control, restricted access, public excluded); the coordination claim (author incentive) is a conceptual reframing that may or may not deliver real author power. The tangled-rope classification holds because the statute maintains genuine coordination function (authors do gain *some* incentive to publish and be registered) while the extraction mechanism persists (the public is still trapped; derivative authors are still excluded).
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, tangled_rope,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 7: COMMON LAW TRADITION (PITON) — The pre-Statute Stationers' Company monopoly (held under common law and custom) was a functional economic institution. By the 1710s, however, that function was degrading: printing costs were declining, distribution networks were expanding, unlicensed printing was increasing, and the crown's censorship power was eroding (post-1695 Licensing Act lapse). The Statute of Anne formalizes a degraded regime — it preserves the monopoly structure through statutory language ('for the encouragement of learning') that obscures that the coordination function has weakened and the extraction mechanism now operates without legitimate structural support. The theater ratio reflects this: much of copyright's legitimacy by 1710 was performative (the 'learning encouragement' frame) rather than addressing the actual economic problem stationers faced.
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 8: NATURAL LAW / UNIVERSAL VIEW (MOUNTAIN) — Reads copyright as emerging from an immutable logic: 'If someone creates intellectual work, they naturally deserve to control and profit from it.' This perspective naturalizes the copyright structure as a universal principle rather than a contingent institutional choice. From this view, the Statute of Anne is merely recognizing a pre-existing right, not creating a new monopoly. The mountain classification is DIAGNOSTICALLY INCORRECT — it represents a false summit where a contingent institutional arrangement (monopoly transfer via statute) is naturalized as a universal principle. The structural data contradicts mountain: the statute creates beneficiaries and victims; it requires active enforcement; it exhibits suppression of alternatives (piracy criminalized). False summit detection engine should flag this.
constraint_indexing:constraint_classification(monopoly_transfer_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(monopoly_transfer_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(monopoly_transfer_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(monopoly_transfer_mechanism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(monopoly_transfer_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(monopoly_transfer_mechanism, TR),
    TR >= 0.70.

:- end_tests(monopoly_transfer_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The statute creates a lasting monopoly that significantly excludes and harms the reading public (trapped at unaffordable prices) and prevents derivative authors from legal creation (trapped by copyright enforcement). However, the statute includes genuine limitations absent in the pre-1710 Stationers' Company monopoly: term limits (28 years for published books, renewable), author reversion rights (copyright reverts to author at term end, enabling new negotiation), and registration requirements (establishing public notice). These limitations reduce pure extraction compared to common-law monopoly (0.68 in 1660). The measurement trajectory reflects this: extractiveness drops from 0.68 to 0.52 as the statute introduces constraints on the monopoly. Suppression (0.65): High. Multiple mechanisms suppress alternatives: piracy is criminalized (legal barrier); printing requires licenses; price monopoly prevents public access (economic barrier); derivative-work prohibition prevents legal alternatives (regulatory barrier). Suppression does not reach 0.90+ (snare level) because some circulation occurs through legal channels and because libraries/privileged groups retain access. Theater ratio (0.58): Moderate-high. The statute's stated rationale ('for the encouragement of learning') is performative relative to actual function — the statute's primary effect is redistributing monopoly legitimacy, not creating new learning incentives for the reading public. However, theater is not extreme (0.70+, piton level) because the 'author incentive' claim has genuine economic force: some authors do gain publishing incentive. The theater value reflects an ambiguity: the statute is sincere about author incentives but misleading about public benefit.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence across eight perspectives spanning all six types. (1-2) The reading public and derivative authors see snare: pure extraction, no exit, trapped by monopoly enforcement. (3-4) Stationers and crown see rope: genuine coordination of their respective problems (economic incentive for stationers; political control for crown). (5) The synchronic reading (became-thinkable) sees tangled rope: the statute creates real author-recognition and incentive coordination alongside persistent monopoly extraction. (6) The diachronic reading (first-held) also sees tangled rope but emphasizes extraction: the statute consolidates stationer control while relabeling it as author protection. (7) The common-law tradition sees piton: the pre-statute monopoly was functionally degrading by 1710 (printing costs declining, distribution expanding, crown power eroding); the statute re-formalizes a degraded regime using 'learning encouragement' language. (8) The natural-law reading sees mountain: copyright emerges as an inevitable, universal principle that authors naturally deserve to control their work. The last perspective is a false summit — the structural data contradicts mountain (beneficiaries, victims, active enforcement required), but the naturalization frame makes it superficially plausible. The gap between perspectives reflects that the statute is genuinely ambiguous: it is simultaneously a reform (adds author recognition, limits monopoly term) and a consolidation (preserves stationer economic power, extends monopoly into statutory law).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value flows from structural position. Beneficiaries with arbitrage options (stationers, crown) derive low d (around 0.15-0.25) from beneficiary status and arbitrage exit, yielding negative or near-zero χ — they experience the constraint as enabling rather than extractive. Powerless agents trapped by monopoly (reading public, derivative authors) derive high d (around 0.90-0.95) from victim status and trapped exit, yielding maximum χ — they experience maximum extraction. Original authors occupy an intermediate position: they are nominally beneficiaries (gained author-rights recognition) but constrained by contracts with stationers, yielding moderate d (around 0.55-0.65). Analytical perspectives derive d from their position relative to the extraction flow (approximately 0.72 for both synchronic and diachronic observers). The synchronic reading, emphasizing conceptual innovation and author-rights emergence, sees coordination function more prominently, which moderates its experienced extraction — the statute genuinely solves author-incentive problems. The diachronic reading, emphasizing institutional consolidation, sees the extraction mechanism persisting beneath conceptual reframing, which elevates its experienced extraction. Both yield tangled rope but with slightly different χ values reflecting their different observational focus.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is not yet mandatrophy-resolved (base_properties.mandatrophy_resolved = false) because the fundamental question remains open: Is copyright a genuine coordination mechanism with asymmetric extraction (tangled rope), or is it a cover story for monopoly maintenance (snare with high theater)? The omega variables identify three critical empirical gaps: (omega_author_power_transfer) Did authors actually gain power or only nominal title? (omega_coordination_vs_cover) Is author incentive a real coordination function or a legitimacy frame? (omega_contingent_vs_inevitable) Was monopoly inevitable or contingent? Until these gaps are resolved, the constraint cannot be confidently classified beyond 'tangled rope.' The mandatrophy emerges because the statute simultaneously creates coordination (author recognition, incentive structure) and extraction (monopoly control, public exclusion), and the relative weight of these cannot be determined from the statute's text alone. The historical record (omega_author_power_transfer through empirical author-stationer contracts) will resolve this. If authors gained real control and compensation, the constraint's coordination function is genuine and mandatrophy is resolved as tangled rope. If stationers retained functional control and authors remained contractually subordinate, the constraint is re-classified as snare with high theater, and mandatrophy resolves via exposure of the cover story. Current state: tangled rope is the holding classification, but this depends on empirical resolution of beneficiary/victim power distribution across the 1710-1750 period (when copyright law matured and author-publisher relationships stabilized).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    became_vs_held_kernel_cut,
    'Is the distinction between ''became thinkable'' (synchronic conceptual emergence) and ''first held'' (diachronic institutional occupation) a genuine structural difference in how copyright operates, or a spurious analytical seam imposed by the observer''s choice of temporal frame?',
    'Historical analysis of author power pre- and post-Statute; comparison of compensation, control, and derivative-work permissions before/after 1710; examination of whether conceptual legitimacy (''author rights'') translated to actual power or merely relabeled stationer control; longitudinal tracking of author agency in contract negotiations with stationers/publishers.',
    'If genuine: two different constraints with different extraction mechanisms (one coordination-focused, one extraction-focused). If spurious: single constraint viewed from two frames; both readings are observers'' artifacts rather than structural realities. Affects whether the story should be decomposed into two separate constraints or kept unified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(became_vs_held_kernel_cut, conceptual, 'Synchronic vs diachronic reading distinction as genuine structural difference or analytical artifact').

omega_variable(
    author_vs_stationer_power_transfer,
    'Did the Statute of Anne actually transfer monopoly power from stationers to authors, or did it transfer monopoly legitimacy while preserving stationer economic control?',
    'Contract analysis: examination of author-stationer agreements pre- and post-1710; empirical study of who held copyright title (stationers typically registered texts); measurement of author compensation rates; analysis of control over derivative works, translations, and editions; longitudinal tracking of author legal standing in copyright disputes.',
    'If authors gained actual power: statute created genuine author-beneficiary class with new incentive structure. If stationers retained control: statute is regulatory capture — relabeled existing monopoly as ''author protection'' to gain legitimacy. Changes the beneficiary declaration and the classification''s authenticity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(author_vs_stationer_power_transfer, empirical, 'Whether copyright power transfer to authors was real or nominal').

omega_variable(
    coordination_vs_cover_story,
    'Is the ''author incentive'' coordination function genuine (the statute actually creates economically meaningful incentives for authors to publish), or is it a cover story legitimizing the continued stationer monopoly?',
    'Econometric analysis: publication rates before/after statute; author composition (paid authors vs. amateur contributors); printing volume and distribution expansion; price movements for published texts; empirical study of whether the statute''s incentive structure enabled author-profitability or merely maintained stationer profitability while providing legal justification.',
    'If genuine: statute is tangled rope (real coordination + extraction). If cover story: statute is snare or scaffold with theatrical legitimacy coat (high theater ratio, deceptive framing). Changes the base properties'' theater_ratio assessment and mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_cover_story, empirical, 'Whether author incentive is genuine coordination function or legitimacy cover').

omega_variable(
    contingent_vs_inevitable_monopoly,
    'Was copyright monopoly in 1710 a contingent institutional choice with historical alternatives (open-source publication norms, patronage models, public subsidy), or was it an inevitable response to the economic problem of expensive printing production?',
    'Historical counterfactual analysis: examination of alternative distribution models that existed (patronage systems in other nations, university-sponsored publication, government-funded printing); comparative study of continental European copyright regimes; analysis of whether monopoly was the minimal viable mechanism or an overreach that could have been replaced by weaker incentive structures.',
    'If contingent: copyright is a policy choice that could have been different; false summit reading is clearly a naturalizing error. If inevitable: copyright may approach mountain-type necessity (the structural economics of printing forced some monopoly). Changes the false-summit interpretation and the natural-law reading''s plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contingent_vs_inevitable_monopoly, conceptual, 'Copyright monopoly as contingent institutional choice vs inevitable economic necessity').

omega_variable(
    statute_as_reform_or_consolidation,
    'Does the Statute of Anne represent a reform that constrained the Stationers'' Company monopoly (reducing extraction via author recognition and limitation of term), or does it represent consolidation that strengthened the monopoly by giving it statutory legitimacy?',
    'Legal historical analysis: comparison of stationers'' powers under common law vs. statute; examination of statutory limitations (term limits, registration requirements, author reversion rights); measurement of enforcement mechanisms before/after; analysis of subsequent litigation patterns and monopoly scope expansion/contraction.',
    'If reform: statute moves constraint toward rope (weaker monopoly, more author power, genuine coordination). If consolidation: statute is piton-to-snare transition (degraded common law monopoly given new statutory armor). Changes the base extractiveness value and the perspectival gap interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statute_as_reform_or_consolidation, empirical, 'Statute of Anne as monopoly reform or monopoly consolidation').

omega_variable(
    reader_welfare_vs_author_incentive,
    'Does copyright law optimally balance reader/public access against author incentives, or does it structurally prioritize author/publisher extraction at reader welfare''s expense?',
    'Economic analysis: measurement of deadweight loss from monopoly pricing; empirical study of access/distribution changes post-statute; comparative analysis of copyright regimes'' impact on literacy, publication volume, and derivative work production; historical evidence of reader advocacy or public resistance to copyright enforcement.',
    'If optimal: copyright is tangled rope with genuine coordination (serves both authors and readers). If prioritizes extraction: copyright is snare from reader perspective, rope only from beneficiary perspective. Changes the mandatrophy resolution and whether the snare classification (perspective 1-2) is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reader_welfare_vs_author_incentive, preference, 'Copyright balance between reader welfare and author incentive as empirical or normative question').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(monopoly_transfer_mechanism, 1660, 1710).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(monopoly_theater_1660, monopoly_transfer_mechanism, theater_ratio, 0, 0.35).
narrative_ontology:measurement(monopoly_theater_1690, monopoly_transfer_mechanism, theater_ratio, 1, 0.45).
narrative_ontology:measurement(monopoly_theater_1710, monopoly_transfer_mechanism, theater_ratio, 2, 0.58).

% Extraction over time
narrative_ontology:measurement(monopoly_extraction_1660, monopoly_transfer_mechanism, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(monopoly_extraction_1690, monopoly_transfer_mechanism, base_extractiveness, 1, 0.6).
narrative_ontology:measurement(monopoly_extraction_1710, monopoly_transfer_mechanism, base_extractiveness, 2, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(monopoly_transfer_mechanism, enforcement_mechanism).
narrative_ontology:affects_constraint(monopoly_transfer_mechanism, author_incentive_problem).
narrative_ontology:affects_constraint(monopoly_transfer_mechanism, printer_capital_recovery).
narrative_ontology:affects_constraint(monopoly_transfer_mechanism, crown_information_control).

% DUAL FORMULATION NOTE:
% The monopoly_transfer_mechanism is the institutional structure binding three upstream constraints: author_incentive_problem (how to motivate authors to create and publish), printer_capital_recovery (how to fund expensive printing production), and crown_information_control (how to maintain political influence over textual circulation). Copyright simultaneously attempts to solve all three, creating embedded tensions. The synchronic reading emphasizes the author-incentive solution; the diachronic reading emphasizes the stationer-capital solution; the piton reading captures the crown-control solution persisting beneath reformed language. Each upstream constraint has different extractiveness; the monopoly_transfer_mechanism's extractiveness (0.52) is an aggregate across three distinct beneficiary flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(monopoly_transfer_mechanism, moderate, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
