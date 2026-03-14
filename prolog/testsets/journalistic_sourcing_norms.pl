% ============================================================================
% CONSTRAINT STORY: journalistic_sourcing_norms
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_journalistic_sourcing_norms, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: journalistic_sourcing_norms
 *   human_readable: Journalistic Sourcing Norms and Source Protection
 *   domain: media/information_governance
 *
 * SUMMARY:
 *   Journalistic sourcing norms — the practices of protecting source
 *   anonymity, maintaining confidentiality agreements, and using 'off the
 *   record' conversations — create a constraint that simultaneously enables
 *   information access and obscures journalistic accountability. The
 *   constraint exhibits hybrid properties: it coordinates between journalists
 *   and sources who would otherwise lack disclosure channels (enabling
 *   marginalized voices and whistleblowing), while simultaneously enabling
 *   powerful institutional actors to shape narratives without accountability
 *   (anonymous government officials, corporate spokespersons). The
 *   extractiveness has increased from 0.38 to 0.52 over forty years as
 *   digital archiving has made anonymity less effective (sources become
 *   identifiable through metadata and contextual analysis) while editorial
 *   pressures have intensified (reducing reporters' capacity to cross-verify
 *   anonymous claims). Theater ratio has risen from 0.42 to 0.58, reflecting
 *   that the professional practice of anonymous sourcing now serves more to
 *   maintain journalistic institutional authority than to serve the stated
 *   function of protecting sources — many sources are identifiable to
 *   sophisticated adversaries despite anonymity agreements, yet the ritual
 *   persists.
 *
 * KEY AGENTS:
 *   - Whistleblowers and Dissidents: Primary victims (powerless/trapped) — structurally dependent on journalist intermediary; cannot verify how their information is framed or used; bear retaliation risk
 *   - Professional Journalists: Primary beneficiary (institutional/arbitrage) — gain access to otherwise-unavailable information; control editorial framing and narrative selection; can invoke professional norms to resist editorial interference
 *   - Institutional Media Outlets: Primary beneficiary (institutional/arbitrage) — benefit from expanded access to powerful sources; use anonymity to protect institutional relationships; avoid direct accountability for source verification
 *   - Marginalized Communities: Secondary victim (powerless/constrained) — may benefit from access to disclosure channels but lack capacity to verify journalistic framing; disproportionately identified when anonymity fails
 *   - Transparency and Access Coalition: Organized agents (organized/constrained) — fact-checking networks, academic researchers, media literacy organizations seeking to replace anonymity with verifiable sourcing
 *   - Government Officials and Corporate Executives: Asymmetric beneficiaries (institutional/arbitrage) — receive anonymity protections while wielding institutional power; use journalists as proxies for narrative management
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(journalistic_sourcing_norms, 0.52).
domain_priors:suppression_score(journalistic_sourcing_norms, 0.65).
domain_priors:theater_ratio(journalistic_sourcing_norms, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(journalistic_sourcing_norms, extractiveness, 0.52).
narrative_ontology:constraint_metric(journalistic_sourcing_norms, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(journalistic_sourcing_norms, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(journalistic_sourcing_norms, tangled_rope).
narrative_ontology:human_readable(journalistic_sourcing_norms, "Journalistic Sourcing Norms and Source Protection").
narrative_ontology:topic_domain(journalistic_sourcing_norms, "media/information_governance").

domain_priors:requires_active_enforcement(journalistic_sourcing_norms).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(journalistic_sourcing_norms, professional_journalists).
narrative_ontology:constraint_beneficiary(journalistic_sourcing_norms, institutional_media_outlets).
narrative_ontology:constraint_victim(journalistic_sourcing_norms, whistleblowers_and_dissidents).
narrative_ontology:constraint_victim(journalistic_sourcing_norms, marginalized_communities).
narrative_ontology:constraint_victim(journalistic_sourcing_norms, public_information_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COERCED WHISTLEBLOWER (SNARE) — Trapped in asymmetric confidentiality agreement with journalist; cannot exit without retaliation risk; bears full extraction cost through anonymity debt and institutional vulnerability. No alternative disclosure channels with equivalent protection. Maximum suppression from institutional power asymmetry.
constraint_indexing:constraint_classification(journalistic_sourcing_norms, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOCAL REPORTER (TANGLED ROPE) — Benefits from sourcing norms that enable access to confidential information and protect their reporting relationships. Also constrained by institutional editorial pressure, audience expectations, and inability to fully corroborate anonymous claims. Experiences both coordination (accessing otherwise-silenced voices) and asymmetric extraction (editors controlling which stories run, sources unable to verify attribution).
constraint_indexing:constraint_classification(journalistic_sourcing_norms, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EDITORIAL INSTITUTION (ROPE) — Primary beneficiary of sourcing norms. Controls narrative framing through source selection and editing; uses anonymity to protect institutional relationships and expand access to powerful actors. Experiences the constraint as pure coordination: protecting sources enables access to otherwise-unavailable information, expanding reportorial capacity.
constraint_indexing:constraint_classification(journalistic_sourcing_norms, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TRANSPARENCY AND ACCESS COALITION (SCAFFOLD) — Organized advocates for source transparency (fact-checking networks, media literacy organizations, academic researchers) see sourcing norms as a temporary coordination failure being systematized as law. The sunset: digital verification methods, blockchain-backed source verification, and decentralized publishing platforms are creating alternative disclosure pathways that require less opacity and provide better auditability. As these mature, traditional anonymity-based sourcing becomes less necessary.
constraint_indexing:constraint_classification(journalistic_sourcing_norms, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNALISTIC OBJECTIVITY RITUAL (PITON) — The sourcing norm of 'protect anonymous sources to maintain objectivity' persists through institutional inertia despite diminished function. In practice, source anonymity enables narrative construction (the journalist selects which anonymous sources appear, how they are framed, what context is included) while the anonymity obscures this construction. The ritual claims to serve transparency but often obscures power relationships. Theater ratio high because the norm is maintained through professional identity and editorial gatekeeping rather than empirical success.
constraint_indexing:constraint_classification(journalistic_sourcing_norms, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, source protection appears as a fundamental structural necessity: any investigative authority must maintain confidentiality to enable access to private information. The anonymity principle seems to be built into the problem of investigation itself. However, this perspective risks naturalizing what is actually a historical institutional arrangement. The constraint's extractiveness and suppression scores suggest the opposite: source protection is a contingent human institution, not a law of nature.
constraint_indexing:constraint_classification(journalistic_sourcing_norms, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(journalistic_sourcing_norms_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(journalistic_sourcing_norms, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(journalistic_sourcing_norms, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(journalistic_sourcing_norms, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(journalistic_sourcing_norms, TR),
    TR >= 0.70.

:- end_tests(journalistic_sourcing_norms_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint enables significant information access (coordination function) but creates asymmetric power dynamics where editors and powerful institutional sources shape narratives while marginalized sources bear retaliation risk and lack verification capacity. The rise from 0.38 to 0.52 reflects that digital forensics have made anonymity less protective while institutional pressures have intensified extraction. The trajectory shows extraction accumulation rather than decay. Suppression (0.65): High. Multiple barriers constrain source mobility: legal liability for breaking confidentiality, career damage for disloyalty, organizational power asymmetries between journalists and institutional sources, and lack of alternative disclosure channels with comparable reach. Theater ratio (0.58): Moderate. The anonymity ritual serves some genuine verification function (protecting sources from retaliation) but increasingly serves institutional performance (maintaining journalistic authority and editorial gatekeeping). Digital verification alternatives (blockchain, cryptographic attribution) are emerging, suggesting the theater function is becoming dominant.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival variance across the six types. The institutional beneficiary (editorial outlet) sees pure coordination (Rope) — anonymity enables access and reporting. The whistleblower sees pure extraction (Snare) — trapped in asymmetric confidentiality with no exit and maximum retaliation risk. The local reporter sees mixed coordination-extraction (Tangled Rope) — benefits from access but constrained by editorial control and source verification limits. The organized transparency advocates see a temporary institutional failure with a sunset (Scaffold) — verifiable sourcing alternatives are emerging. The journalistic objectivity ritual sees itself as degraded (Piton) — the anonymity norm persists through professional identity despite diminished protective function. The civilizational analytical observer risks seeing immutable law (Mountain) — investigation requires confidentiality — but the structural data reveals this as a false summit: the extractiveness and suppression metrics show the constraint is contingent, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) derives from the agent's position in the sourcing constraint. Powerless whistleblowers trapped in anonymity agreements experience high extraction (d ≈ 0.92, constrained by trapped exit options and victim status). Professional journalists with institutional backing and arbitrage options experience low or negative extraction (d ≈ 0.15, beneficiary status + arbitrage exit → they can navigate to competing outlets or independent platforms). The institutional media outlet experiences negative extraction — the constraint subsidizes their access and reduces accountability (d ≈ 0.10). The analytical observer at civilizational scope risks naturalizing this arrangement as an inherent feature of investigative journalism (d ≈ 0.72 for 'natural law' interpretation), but the structural data reveals it as a contingent institutional arrangement vulnerable to verification alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that sourcing norms serve dual functions — coordination (enabling marginalized voices access to disclosure channels) and extraction (enabling institutional actors to shape narratives without accountability). The constraint cannot be classified as pure Rope (coordination) because of the asymmetric extraction; it cannot be classified as pure Snare (extraction) because genuine coordination occurs. Tangled Rope is the correct canonical classification: the constraint has BOTH genuine coordination function (some marginalized sources genuinely gain access who would otherwise have none) AND asymmetric extraction (institutional sources receive protection while marginal sources bear retaliation risk). The mandatrophy reveals that the coordination function is real but insufficient to justify the extraction asymmetry. Resolution requires either (a) symmetric source protection across power levels, or (b) transition to verification-based sourcing that eliminates anonymity-enabled narrative construction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anonymous_source_identity_recovery,
    'What methods distinguish effective source protection from plausible deniability that obscures journalistic accountability?',
    'Comparative analysis of source protection success rates across publication contexts; historical cases where anonymity genuinely prevented retaliation vs. where it enabled false reporting; feasibility studies of cryptographic source verification',
    'If effective: sourcing norms genuinely enable marginalized voices (Rope function strengthens). If plausible deniability: anonymity obscures journalistic power and enables narrative construction without accountability (Snare function dominates).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anonymous_source_identity_recovery, empirical, 'Whether anonymity enables accountability or obscures it').

omega_variable(
    institutional_source_asymmetry,
    'Do powerful institutional sources (government officials, corporate executives) experience different protection and verification standards than marginalized sources?',
    'Corpus analysis of sourcing patterns in national media; comparison of anonymity protections granted to official vs. dissenting sources; audit of which anonymous sources drive editorial decisions',
    'If symmetric: sourcing norms serve coordination across power levels (Rope). If asymmetric: norms systematically favor institutional actors while marginalizing dissidents (Snare for powerless, Rope for institutional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_source_asymmetry, empirical, 'Whether sourcing protections are symmetric across power levels').

omega_variable(
    alternative_verification_sufficiency,
    'Do emerging verification technologies (blockchain source records, cryptographic attribution, distributed fact-checking networks) provide adequate alternatives to anonymity-based sourcing?',
    'Pilot programs using transparent source tracking; comparison of claim accuracy and source reliability between traditional anonymity and verifiable alternatives; adoption metrics for new protocols',
    'If sufficient: scaffold sunset clause is viable — transition timeline becomes measurable. If insufficient: traditional anonymity remains necessary, and sunset clause is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_verification_sufficiency, empirical, 'Whether transparent verification can replace anonymity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.65) structural (legal liability, editorial gatekeeping, institutional power asymmetry) or internalized (journalists self-censor to maintain access, sources internalize institutional framing)?',
    'Exit trajectory analysis: sources who break with institutions report reduced suppression or persistent self-censorship post-exit; journalist interviews about editorial pressure vs. internalized professional norms',
    'If structural: suppression drops post-exit. If internalized: suppression persists after institutional contact ceases — the constraint is psychological rather than material.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(journalistic_sourcing_norms, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsn_tr_t0, journalistic_sourcing_norms, theater_ratio, 0, 0.42).
narrative_ontology:measurement(jsn_tr_t20, journalistic_sourcing_norms, theater_ratio, 20, 0.5).
narrative_ontology:measurement(jsn_tr_t40, journalistic_sourcing_norms, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(jsn_be_t0, journalistic_sourcing_norms, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(jsn_be_t20, journalistic_sourcing_norms, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(jsn_be_t40, journalistic_sourcing_norms, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(journalistic_sourcing_norms, information_standard).
narrative_ontology:affects_constraint(journalistic_sourcing_norms, institutional_media_gatekeeping).
narrative_ontology:affects_constraint(journalistic_sourcing_norms, whistleblower_protection_asymmetry).

% DUAL FORMULATION NOTE:
% Journalistic sourcing norms decompose into institutional media gatekeeping (how editorial institutions select and frame anonymous sources) and whistleblower protection asymmetry (how power asymmetries determine whose anonymity is protected). This story addresses the constraint itself; downstream stories address how it manifests in specific institutional contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(journalistic_sourcing_norms, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
