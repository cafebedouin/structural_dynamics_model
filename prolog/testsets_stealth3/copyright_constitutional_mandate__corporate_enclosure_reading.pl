% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_constitutional_mandate__corporate_enclosure_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright Maximalist Mandate — Corporate Enclosure Reading
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The U.S. Copyright Clause promises exclusive rights 'for limited Times'
 *   in exchange for promoting the progress of science and the useful arts.
 *   The corporate enclosure reading holds that copyright is a property right
 *   demanding maximal protection and that 'limited Times' therefore licenses
 *   extension to the verge of perpetuity — a reading operationalized over the
 *   last half-century as serial term extension (including retroactive
 *   extension of existing works), criminalized circumvention of technical
 *   protections, and progressive narrowing of fair use through enforcement
 *   practice and platform filtering. The standing arrangement under contest
 *   is that maximal-protection regime as it actually operates. KEY AGENTS (by
 *   structural relationship): - major_content_conglomerates: Primary
 *   beneficiary and agenda driver (institutional/arbitrage) — collects
 *   extended-term licensing streams, drafts the legislative agenda -
 *   us_congress_and_ustr: Formal agenda setter (institutional/constrained) —
 *   enacts extensions, exports the standard through trade agreements;
 *   financed by the beneficiary bloc - platform_intermediaries:
 *   Dual-positioned enforcement subcontractor (institutional/arbitrage) —
 *   runs filtering, pays licensing, gains a scale moat -
 *   legacy_rights_holder_estates: Secondary beneficiary (organized/mobile) —
 *   collects on inherited catalogs - ip_enforcement_industry: Secondary
 *   beneficiary (organized/mobile) — sells detection and litigation services
 *   - derivative_creators: Primary payer (moderate/constrained) — bears
 *   takedown, damages, and clearance burdens - educators_and_researchers:
 *   Payer (moderate/constrained) - libraries_and_archives: Payer
 *   (organized/trapped) - software_interoperability_developers: Payer
 *   (moderate/constrained) — liability attaches to the tool - general_public:
 *   Payer (organized/trapped) — diminished public domain, episodic
 *   mobilization - copyright_reform_scholars: Analytical observer
 *   (organized/analytical) - future_creators: Excluded (powerless/trapped) -
 *   the_public_domain: Non-agent entity listed for completeness.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.78).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright Maximalist Mandate — Corporate Enclosure Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '5e19f444-f10a-41d4-9ad0-fede154ee7a1').
narrative_ontology:cs_kernel_codification('5e19f444-f10a-41d4-9ad0-fede154ee7a1', fixed_text).
narrative_ontology:cs_authority_grounding('5e19f444-f10a-41d4-9ad0-fede154ee7a1', extraction).
narrative_ontology:cs_interpretation_layer_present('5e19f444-f10a-41d4-9ad0-fede154ee7a1').
narrative_ontology:cs_reading_relation('5e19f444-f10a-41d4-9ad0-fede154ee7a1', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('5e19f444-f10a-41d4-9ad0-fede154ee7a1', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('5e19f444-f10a-41d4-9ad0-fede154ee7a1', foundational, copyright_is_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('5e19f444-f10a-41d4-9ad0-fede154ee7a1', copyright_is_natural_property_right, deontological).
narrative_ontology:cs_axiom('5e19f444-f10a-41d4-9ad0-fede154ee7a1', foundational, limited_times_imposes_no_substantive_ceiling).
narrative_ontology:cs_axiom_status(limited_times_imposes_no_substantive_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('5e19f444-f10a-41d4-9ad0-fede154ee7a1', limited_times_imposes_no_substantive_ceiling, conventional).
narrative_ontology:cs_reference_frame('5e19f444-f10a-41d4-9ad0-fede154ee7a1', maximal_property_protection_baseline).
narrative_ontology:cs_drift_state('5e19f444-f10a-41d4-9ad0-fede154ee7a1', contemporary_post_ctea_dmca_era, gap(revival_pressure, minor, true)).
narrative_ontology:cs_created_at('5e19f444-f10a-41d4-9ad0-fede154ee7a1', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_content_conglomerates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_rights_holder_estates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, ip_enforcement_industry).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, libraries_and_archives).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, software_interoperability_developers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, general_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, us_congress_and_ustr).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, platform_intermediaries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, platform_intermediaries).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_as_natural_property_doctrine).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, incentive_justification_rhetoric).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, rational_basis_term_deference_precedent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own and exploit deep film, music, and publishing catalogs. Draft and fund the legislative agenda through lobbying, campaign contributions, and trade-association positions, and negotiate enforcement standards through trade-agreement processes. Collect licensing fees, extended-term royalties, and streaming minimum guarantees on works whose terms have been repeatedly lengthened. They can relocate IP holdings, choose favorable jurisdictions, and select enforcement forums, so exit from any single national regime is available to them in a way it is not to the parties who pay.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_content_conglomerates, agenda_setter,
    institutional, generational, arbitrage, global).

% Formally enact copyright term and enforcement law; the trade representative negotiates the agreement chapters that export the standard abroad. Campaign finances and home-district employment ties run heavily through the beneficiary industries. Members' personal horizons run to the next election cycle, while the term extensions they vote for run decades past their tenure.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, us_congress_and_ustr, agenda_setter,
    institutional, immediate, constrained, national).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, us_congress_and_ustr, beneficiary).

% Operate the large user-generated-content and streaming platforms. Build and run the filtering and notice-and-takedown machinery the enforcement regime requires, and pay substantial licensing sums to rights holders. Simultaneously gain a competitive moat, because filtering obligations raise fixed costs for smaller entrants. Their compliance infrastructure has become a core organizational capability they neither originally planned nor can cheaply shed.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, platform_intermediaries, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, platform_intermediaries, payer).

% Hold inherited rights on works by long-dead authors and composers. Collect royalties on catalogs whose terms have been extended repeatedly since acquisition, often held by heirs two and three generations removed from any creator. Their income requires no ongoing production; exit consists of selling or relicensing the catalog.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_rights_holder_estates, beneficiary,
    organized, generational, mobile, global).

% Anti-piracy vendors, forensic-watermarking firms, takedown-service providers, and specialized litigation practices. Revenue scales with the breadth and complexity of enforcement obligations; every new protection scheme enlarges the addressable market. Mobile across clients and jurisdictions.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, ip_enforcement_industry, beneficiary,
    organized, biographical, mobile, global).

% Remix musicians, video essayists, fan creators, documentary filmmakers, and sample-based producers who build on existing recordings and footage. Face takedown notices, demonetization, statutory-damages exposure, and clearance costs that scale with term length. Their distribution runs through platforms whose filtering systems err toward removal; working outside the rights system means exiting mainstream distribution.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    moderate, biographical, constrained, global).

% Teach and publish with films, music, images, and texts under copyright. Face licensing fees for course materials, takedown of instructional media, and fair-use boundaries that narrow as enforcement tightens. Open-educational-resource channels exist but cover a shrinking share of the contemporary materials students need.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_researchers, payer,
    moderate, generational, constrained, global).

% Preserve and lend the published record. Mission-bound to hold complete collections, so there is no alternative corpus to migrate to. Preservation copying of obsolete media, orphan-works access, and digital lending all run into term length and anticircumvention restrictions; their professional associations litigate and lobby continuously.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, libraries_and_archives, payer,
    organized, civilizational, trapped, global).

% Build compatibility layers, emulators, security tools, and preservation software that interact with protected works and technical protection measures. Anticircumvention provisions attach liability to the tool regardless of use, so research and interoperability work carries legal exposure that follows the artifact across borders. Skills are portable; the exposure attaches to the work product, not the worker.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, software_interoperability_developers, payer,
    moderate, biographical, constrained, global).

% Audiences, purchasers, and users of culture. Bear higher prices, delayed or never-arriving public-domain releases, and a shrinking shared stock of freely reusable works. Individually dispersed but capable of episodic mass mobilization — the 2012 SOPA blackout and the 2019 Article 13 protests — which has stopped bills but never shortened a term. There is no exiting the culture one lives inside.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, general_public, payer,
    organized, generational, trapped, global).

% Legal scholars, economists, and digital-rights organizations who map the arrangement's flows, litigate challenges, and propose rebalancings. Hold no administrative power; their leverage is analysis, court filings, and agenda disruption.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_reform_scholars, observer,
    organized, generational, analytical, global).

% Creators not yet working who will inherit today's culture as raw material. Locked out prospectively by terms that will still be running decades from now, and represented by no one in current negotiations. They cannot exit a culture they have not yet entered.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, future_creators, excluded,
    powerless, civilizational, trapped, global).

% The shared stock of works free for anyone to use. Grows more slowly than it would under shorter terms and loses would-be entrants to repeated extensions. Listed for completeness; it acts through no one, though archivists and scholars speak for it intermittently.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, the_public_domain, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(copyright_constitutional_mandate__corporate_enclosure_reading, the_public_domain).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, major_content_conglomerates).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the ex ante incentive problem for producing and publishing new works — giving creators and their financiers a window of exclusive exploitation — and operates standardized licensing markets (reproduction, mechanical, performance, synchronization) so that large-scale reuse can be cleared and paid; enforcement coordination harmonizes these expectations across jurisdictions.
% TRANSFER_FUNCTION: Moves money from anyone who copies, performs, teaches with, preserves, or builds on recorded culture — audiences, derivative creators, schools, libraries, software developers — to whoever holds the relevant exclusive rights. Retroactive term extensions move royalty streams on already-completed works with no additional production exchanged in return.
% ABSENT_VOICES: Future creators who will want to build on today's culture, and the audiences of the would-be public domain, have no seat at any negotiating table; their interests appear only as projections by scholars and archivists. Non-industry creators were absent from the term-extension and anticircumvention drafting processes; in the recent EU filtering directive, platforms and rights holders sat in the trilogue while individual creators and user communities were represented only by proxies.
% DISAPPEARANCE_RATIONALE: If the maximalist mandate vanished overnight — terms reverting to short fixed windows, circumvention decriminalized, fair use restored as a robust defense — licensing markets would reprice downward, enforcement-industry demand would collapse, platform filtering obligations would lapse, catalog values built on near-century terms would write down sharply, and a wave of twentieth-century culture would begin entering public use on schedule. The rearrangement is precisely what the beneficiary seats spend lobbying budgets to prevent.
% FOUNDING_PROBLEM: The Anglo-American copyright bargain codified in the Statute of Anne and the U.S. Copyright Clause: how to secure authors enough exclusive right to motivate writing and publication while ensuring works ultimately pass into the public stock — 'limited Times' was written as the bargain's load-bearing limit.
% FOUNDING_PROBLEM_CORROBORATION: Industry rightsholders attest the incentive problem is live and cite ongoing infringement. Corroboration from outside the benefiting parties cuts the other way for the maximalist increment: empirical economics commissioned at arm's length from industry (the Gowers Review's supporting analysis, the Hargreaves Review, Congressional Research Service summaries) finds term extensions beyond roughly life-plus-fifty add negligible production incentive because distant royalties discount to near zero, and the recorded judicial findings on retroactive extension establish that it cannot operate as an incentive at all. No attester outside the beneficiary set maintains that the retroactive component solves a live problem.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored high (0.82) because the mandate's distinctive operations — retroactive term extension, circumvention criminalization, fair-use narrowing — transfer value or foreclose reuse independently of any new production. Suppression (0.78) is authored as a raw structural property (the engine scales only extractiveness, by directionality and scope): statutory damages, anticircumvention liability, border and domain seizures, treaty ratchets, and platform filtering, with a chilling-effect layer on top handled by omega. Theater (0.58) crosses the Goodhart line: the incentive-to-artists rationale now chiefly justifies activity that returns little to working artists — retroactive extensions pay estates and catalogs, not creators. Accessibility_collapse (0.48) is mid-range: parallel channels (open licensing, open access) persist, but reuse of in-copyright culture largely closes once the enforcement reality is understood. Resistance (0.60) is sustained and occasionally wins tactical victories (the 2012 bill withdrawal, amendments to the EU filtering directive) but has never reversed a term extension. The measurement series run on one shared grid (decade steps across the 1976–2026 anchor interval) so every tracked metric is authored at every examined point. The trajectories are ratchet-shaped rather than cyclical: each adoption crisis — videotape, file-sharing, streaming, AI training — supplies the occasion for the next upward step, and no phase relaxes; the ratchet itself, not oscillation, is the dynamic. Claim and metrics are authored independently: I claim tangled_rope because genuine ex ante incentive and licensing coordination survive alongside the asymmetric transfer; the rising series is the data that would date a transition toward snare.
 *
 * PERSPECTIVAL GAP:
 *   From the conglomerate seat the arrangement computes as coordination it financed and won: property defended, catalogs capitalized, enforcement delivered. From the derivative-creator and archive seats the same statutes compute as enforced extraction with suppressed exits. Congress's seat is the pivot: formally the setter, structurally financed by the beneficiary side, which is why the body that wrote 'limited Times' enacts maximal readings of it. Platform intermediaries sit doubly — licensing payer and enforcement beneficiary — and show a mild institutional identity lock: their compliance infrastructure has become constitutive of how the organization understands itself, so they defend filtering obligations even where licensing costs exceed the moat's value. Among same-power institutional actors, exit differentiates the experience: conglomerates arbitrage across IP domiciles and enforcement forums, while platforms arbitrage jurisdictionally but are anchored by infrastructure they cannot relocate.
 *
 * DIRECTIONALITY LOGIC:
 *   Conglomerates, estates, and the enforcement industry hold beneficiary positions (low d): the mandate subsidizes them through extended exclusivity, and their arbitrage-grade exit places them nearest the subsidy end. Derivative creators, educators, archives, interoperability developers, and the public hold payer positions (high d), with exit modulation doing real work: archives are trapped (no alternative corpus exists), the public is trapped inside its own culture, developers are constrained because liability follows the artifact, and derivative creators are constrained by platform dependence. Congress is formally an agenda setter but campaign-financed by the beneficiary bloc; the structural declarations place it near the beneficiary end without needing an override. Global spatial scope raises verification difficulty and amplifies effective extraction on the payer seats, whose exposure follows works across borders while their remedies remain national. No directionality overrides are used: the beneficiary/victim declarations plus exit options already yield the correct ordering, and an override keyed to the institutional power atom would misapply across conglomerates, platforms, and Congress, which this story deliberately distinguishes by declaration rather than by correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding bargain splits cleanly: the forward-looking incentive half is live for new commercial works; the retroactive-extension half is dead as incentive policy and persists as transfer. Claiming tangled_rope keeps the live coordination half visible — a snare label would erase the real ex ante incentive and licensing functions that new creation still relies on — while the theater_ratio series crossing 0.5 marks the incentive rationale's conversion into cover for the dead half. On the R5 mismatch consumer: founding_problem_status=contested crossed with disappearance_verdict=world_rearranges fires no zombie flag, correctly, because the parties genuinely dispute liveness; the signal lives instead in the corroboration asymmetry — independent economics attests the retroactive component solves nothing, while only the beneficiaries attest otherwise — which is the fingerprint of a dead half carried by a live half's reputation. The classification thus prevents two opposite mislabels: reading the whole arrangement as pure extraction (denying the surviving coordination new works still use) and reading it as pure coordination (denying the documented transfer with no production exchanged).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the copyright_constitutional_mandate kernel; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three reading files: the public_scaffold_reading flips the beneficiary set toward the reading public and future creators and drops epsilon; the judicial_ambiguity_reading strips the maximalist mandate and leaves procedural deference with low structural extraction.',
    'Under the scaffold reading, beneficiaries become the public and derivative creators, incumbent holders become the paying side, and the type moves toward scaffold/rope; under the ambiguity reading the constraint loses its substantive mandate entirely. The disagreement is located in the meaning of ''limited Times'' (substantive ceiling versus formal boundary) and in whether the Clause''s property character or its public-purpose character dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file is the corporate-enclosure member of a three-reading kernel family.').

omega_variable(
    maximalist_increment_share,
    'What share of the measured extraction is attributable to the maximalist increment (extensions beyond baseline terms, anticircumvention liability, fair-use narrowing) rather than to baseline copyright coordination?',
    'Counterfactual econometrics comparing creation, licensing, and reuse outcomes under simulated baseline terms (fixed moderate terms, no anticircumvention liability) using panel variation across jurisdictions and eras.',
    'If the increment carries most of the extraction, the tangled_rope claim is generous and the constraint is snare-dominant; if baseline copyright carries it, the enclosure reading inherits responsibility for a broader institutional profile than its own mandate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximalist_increment_share, empirical, 'Attribution of measured extraction between the maximalist increment and baseline copyright.').

omega_variable(
    retroactive_extension_incentive_null,
    'Do retroactive term extensions produce any measurable production incentive, as their justificatory rhetoric requires?',
    'Panel studies of creative output around the 1976 and 1998 extension events, controlling for technology trends; incentive theory predicts a null effect for already-created works.',
    'A confirmed null converts the retroactive component''s justification from incentive provision to pure transfer, raising the functional reading of theater_ratio and supporting the dead-half assessment in the mandatrophy analysis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retroactive_extension_incentive_null, empirical, 'Whether the retroactive component of term extension does any incentive work.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (statutory damages, anticircumvention liability, filtering mandates) or internalized (chilling effects — projects abandoned before any legal contact)?',
    'Survey documentary filmmakers, remix musicians, and security researchers for forgone projects versus received notices; compare self-censorship rates across jurisdictions with identical statutes but different enforcement intensity.',
    'If internalized suppression is substantial, effective suppression exceeds the structural measure and persists after statutory reform; reform would under-deliver relative to its paper effect, and the omega routes the surplus to the internalized mechanism rather than to enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized share of the suppression burden on creators and researchers.').

omega_variable(
    treaty_ratchet_exit_lock,
    'Do international minimum-term obligations and trade-dispute mechanics make unilateral term reduction practically unavailable to a signatory — is national-level exit locked?',
    'Trade-law analysis of withdrawal costs, retaliation exposure, and precedent: no signatory has shortened terms below the international minima, and attempted reductions have been blocked at review.',
    'If exit is locked, fixing_cost stays prohibitive regardless of domestic politics, and the constraint''s persistence is supranational rather than a matter of legislative preference — the classification consequence concentrates on the agenda_setter seat''s effective discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_ratchet_exit_lock, empirical, 'Whether treaty architecture removes the national exit option on term length.').

omega_variable(
    fair_use_separability_from_property_frame,
    'Is robust fair use structurally separable from the property-maximal reading, or does narrowing fair use follow necessarily once copyright is treated as a full property right?',
    'Doctrinal comparison across property-framed and utility-framed jurisdictions: does the strength of the property framing predict fair-use and fair-dealing breadth independent of industry concentration?',
    'If inseparable, fair-use restriction is intrinsic to this reading rather than an abuse of it — the reading''s own logic generates the victim set; if separable, the narrowing is a contingent political achievement and reformable within the frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_separability_from_property_frame, conceptual, 'Whether the victim-generating moves are internal to the property-maximal frame.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ccm_corp_enclosure_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.27).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t0, observed).
narrative_ontology:measurement(ccm_corp_enclosure_tr_t10, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 10, 0.34).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t10, observed).
narrative_ontology:measurement(ccm_corp_enclosure_tr_t20, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 20, 0.43).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t20, observed).
narrative_ontology:measurement(ccm_corp_enclosure_tr_t30, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 30, 0.51).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t30, observed).
narrative_ontology:measurement(ccm_corp_enclosure_tr_t40, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 40, 0.56).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t40, observed).
narrative_ontology:measurement(ccm_corp_enclosure_tr_t50, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement_basis(ccm_corp_enclosure_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(ccm_corp_enclosure_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.46).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t0, observed).
narrative_ontology:measurement(ccm_corp_enclosure_be_t10, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 10, 0.53).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t10, observed).
narrative_ontology:measurement(ccm_corp_enclosure_be_t20, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t20, observed).
narrative_ontology:measurement(ccm_corp_enclosure_be_t30, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 30, 0.71).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t30, observed).
narrative_ontology:measurement(ccm_corp_enclosure_be_t40, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 40, 0.77).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t40, observed).
narrative_ontology:measurement(ccm_corp_enclosure_be_t50, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 50, 0.82).
narrative_ontology:measurement_basis(ccm_corp_enclosure_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(ccm_corp_enclosure_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t0, observed).
narrative_ontology:measurement(ccm_corp_enclosure_su_t10, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 10, 0.47).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t10, observed).
narrative_ontology:measurement(ccm_corp_enclosure_su_t20, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t20, observed).
narrative_ontology:measurement(ccm_corp_enclosure_su_t30, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 30, 0.69).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t30, observed).
narrative_ontology:measurement(ccm_corp_enclosure_su_t40, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t40, observed).
narrative_ontology:measurement(ccm_corp_enclosure_su_t50, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement_basis(ccm_corp_enclosure_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the copyright clause / copyright mandate' decomposes into three structurally distinct readings of one kernel (copyright_constitutional_mandate). This file instantiates the corporate_enclosure_reading, whose epsilon is high because its beneficiary/victim structure concentrates gains in incumbent holders and costs in derivative creators, educators, archives, and the public. The public_scaffold_reading shares the referent (the standing arrangement) but authors low epsilon and a scaffold/rope profile because it reads the monopoly as transitional means to a public-domain end; the judicial_ambiguity_reading authors low structural extraction because it reduces the constraint to procedural deference. The upstream member with the most established empirical footing influences the others: each ratified extension hardens the deference precedent the ambiguity reading rests on, and each extension's justification borrows the incentive rhetoric the scaffold reading must rebut. All three files link one another via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
