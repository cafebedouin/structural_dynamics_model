% ============================================================================
% CONSTRAINT STORY: institutional_evidence_disclosure_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_institutional_evidence_disclosure_asymmetry, []).

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
 *   constraint_id: institutional_evidence_disclosure_asymmetry
 *   human_readable: Institutional Evidence Disclosure Asymmetry
 *   domain: governance/institutional_accountability
 *
 * SUMMARY:
 *   Institutional evidence disclosure asymmetry describes the structural
 *   constraint where controlling institutions can selectively withhold,
 *   time-delay, or filter evidence from external stakeholders while retaining
 *   full epistemic access to complete information. This creates a durable
 *   extraction mechanism: the institution benefits from information monopoly
 *   (narrative control, timing advantage, interpretation dominance), while
 *   external stakeholders bear the cost of opacity (forced reliance on
 *   filtered narratives, inability to independently verify claims, suppressed
 *   accountability). The constraint combines genuine coordination
 *   functions—institutions may legitimately need some confidentiality for
 *   operational security, privacy protection, or classified information—with
 *   asymmetric extraction. The theater ratio (0.68) reflects that formal
 *   disclosure processes often produce performative compliance: redacted
 *   documents, delayed releases, selective declassification, and compliance
 *   rituals that create appearance of transparency without substantive
 *   evidence access. The rising theater ratio over time indicates increasing
 *   sophistication of institutional opacity mechanisms even as transparency
 *   mandates formally strengthen.
 *
 * KEY AGENTS:
 *   - Institutional Gatekeepers: Primary beneficiary (institutional/arbitrage) — controls information flow, narrative timing, evidence interpretation. Maximum exit optionality.
 *   - External Stakeholders: Primary victim (powerless/trapped) — forced reliance on institutionally-filtered evidence. No independent verification capacity. No exit options.
 *   - Oversight Institutions: Secondary institutional actor (institutional/constrained) — mandated to verify but dependent on target institution for evidence access. Asymmetric power relation.
 *   - Investigative Researchers: Secondary victim (moderate/constrained) — need institutional cooperation for evidence access; also benefit from institutional credibility and methodology.
 *   - Transparency Advocacy Coalition: Organized agent (organized/constrained) — building alternative pathways through mandatory disclosure timelines, open-data standards, FOIA strengthening.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional opacity as necessary confidentiality requirement.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(institutional_evidence_disclosure_asymmetry, 0.58).
domain_priors:suppression_score(institutional_evidence_disclosure_asymmetry, 0.65).
domain_priors:theater_ratio(institutional_evidence_disclosure_asymmetry, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(institutional_evidence_disclosure_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(institutional_evidence_disclosure_asymmetry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(institutional_evidence_disclosure_asymmetry, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(institutional_evidence_disclosure_asymmetry, tangled_rope).
narrative_ontology:human_readable(institutional_evidence_disclosure_asymmetry, "Institutional Evidence Disclosure Asymmetry").
narrative_ontology:topic_domain(institutional_evidence_disclosure_asymmetry, "governance/institutional_accountability").

domain_priors:requires_active_enforcement(institutional_evidence_disclosure_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(institutional_evidence_disclosure_asymmetry, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(institutional_evidence_disclosure_asymmetry, information_monopolists).
narrative_ontology:constraint_victim(institutional_evidence_disclosure_asymmetry, external_stakeholders).
narrative_ontology:constraint_victim(institutional_evidence_disclosure_asymmetry, field_epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: POWERLESS EXTERNAL STAKEHOLDER (SNARE) — Trapped by institutional opacity. No access to evidence withheld by the controlling institution. Cannot verify claims, cannot exit the information dependency. Experiences maximum extraction: forced reliance on institutionally-filtered narratives without independent verification capacity.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INVESTIGATIVE RESEARCHER (TANGLED ROPE) — Constrained by selective disclosure and archive access restrictions. Also benefits from institution's credibility, methodology, and data availability (when disclosed). Mixed coordination-extraction: genuine need for institutional coordination with asymmetric information asymmetry that enables extraction.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INSTITUTIONAL GATEKEEPER (ROPE) — Experiences selective disclosure as pure coordination mechanism. Controls information flow to manage narrative, timing, and interpretation. Net beneficiary with full exit optionality: can choose what to reveal and when. Extraction flows toward this agent.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TRANSPARENCY ADVOCACY COALITION (SCAFFOLD) — Organized agents (FOIA advocates, open-government networks, civil society watchdogs) see disclosure asymmetry as a solvable coordination failure with a sunset: transparency mandates, mandatory disclosure timelines, and open-data standards are incrementally replacing selective disclosure. Temporal constraint with declining suppression over generational timescale.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OVERSIGHT INSTITUTION (TANGLED ROPE) — Theoretically mandated to verify institutional claims, but constrained by resource limitations, political pressure, and dependence on the target institution's cooperation for evidence access. Coordinating function (accountability) exists alongside extraction vulnerability: the oversight body's effectiveness depends on disclosure by the very institution it oversees, creating asymmetric power relation.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PERFORMATIVE DISCLOSURE RITUAL (PITON) — Institutional disclosure statements, compliance reports, and transparency theater persist through inertia despite minimal functional verification. Disclosure rituals create appearance of accountability without substantive evidence access. Theater ratio high because compliance documentation is largely performative: checkboxes and redactions replace actual evidence sharing. The mechanism degrades but persists because formal processes avoid accountability.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some informational asymmetry appears inherent to institutional coordination: institutions cannot disclose everything without compromising operational security, privacy, or legitimate confidentiality. This perspective naturalizes what is actually a contingent institutional choice to maximize gatekeeping power. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(institutional_evidence_disclosure_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(institutional_evidence_disclosure_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(institutional_evidence_disclosure_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(institutional_evidence_disclosure_asymmetry, TR),
    TR >= 0.70.

:- end_tests(institutional_evidence_disclosure_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The institutional gatekeeper captures significant benefits from information monopoly—narrative control, timing advantage, ability to frame evidence interpretation—but the constraint is not maximally extractive because some legitimate confidentiality needs exist and some disclosure does occur. The value reflects that a substantial portion of the asymmetry is strategic gatekeeping rather than necessary secrecy. Suppression (0.65): High. Significant barriers to independent verification include: formalized access restrictions, classification systems, redaction authority, archive control, and institutional culture of opacity. Stakeholders face high costs to obtain competing evidence and face institutional resistance to transparency. Theater ratio (0.68): High and rising. Formal disclosure processes produce theater: compliance reports with redactions, selective declassification on institutional timelines, and accountability rituals that create appearance of transparency. The ratio rises over time because institutions have become more sophisticated at performative compliance—they now can point to disclosure mechanisms while maintaining effective gatekeeping. This is classic Piton dynamics: the mechanism degrades (actual disclosure doesn't increase) but the ritual elaborates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same institutional mechanism appears as coordination to the gatekeeper (Rope—managing information flow is a legitimate coordination function), as mixed coordination and extraction to moderate actors who need institutional cooperation (Tangled Rope—they benefit from institutional resources but suffer from asymmetric information), as a temporary problem with a sunset to organized advocates (Scaffold—mandatory disclosure timelines and open-data standards are reducing asymmetry), as pure extraction to powerless stakeholders locked out of information (Snare—trapped reliance on filtered narratives), and as degraded ritual to the institution itself (Piton—performance of transparency without substantive function). The false summit at the analytical/civilizational level naturalizes institutional opacity as inherent to security and privacy, missing that the asymmetry is a contingent choice to maximize gatekeeping.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the disclosure asymmetry. Institutional gatekeepers with arbitrage options (can choose what to disclose when) experience low d values (0.05-0.15) because disclosure is discretionary—they benefit from withholding. External stakeholders with no alternative information sources experience high d values (0.85-0.95) because they are forced to depend on institutional disclosure. Moderate agents like investigators occupy intermediate positions (d ≈ 0.55-0.65) because they have some alternative research methods but substantial dependence on institutional cooperation. Oversight institutions with constrained exit options (d ≈ 0.60-0.70) because they face political pressure and resource limitations that constrain their independent verification capacity. The asymmetry in d values across perspectives is the diagnostic signature of extraction: some agents have discretion while others have none.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_threshold_ambiguity,
    'What proportion of withheld evidence represents legitimate confidentiality needs versus strategic gatekeeping?',
    'Comparative analysis of disclosure patterns across institutions with different transparency mandates; post-disclosure review of originally withheld materials to assess whether secrecy was justified',
    'If legitimacy_ratio > 0.70: constraint reclassifies toward Rope (more coordination, less extraction). If legitimacy_ratio < 0.30: constraint reclassifies toward Snare (pure extraction with opacity cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_threshold_ambiguity, empirical, 'Proportion of withheld evidence that is legitimately confidential versus strategically gatekept').

omega_variable(
    asymmetry_extraction_coupling,
    'Does the disclosure asymmetry actively extract value (asymmetric benefit to gatekeeper) or merely protect existing benefits from scrutiny?',
    'Timeline analysis: does the institution gain advantage from delayed disclosure? Do timing patterns correlate with institutional interests? Analysis of what decisions are made before vs after disclosure.',
    'If actively extractive: higher chi, validates Snare perspective. If protective: lower chi, reclassifies toward Piton (performative rather than extractive).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(asymmetry_extraction_coupling, empirical, 'Whether disclosure asymmetry actively extracts value or protects existing benefits').

omega_variable(
    multi_stakeholder_benefit_distribution,
    'Do some external stakeholders benefit from selective disclosure (e.g., preferred partners get early access), or is the asymmetry purely unilateral extraction?',
    'Pattern analysis of who gains access to withheld information and when; identification of preferential disclosure to allied stakeholders',
    'If multi-stakeholder benefits: constraint fragments into multiple stories (preferred partners experience Rope, excluded stakeholders experience Snare). If purely unilateral: Snare/Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(multi_stakeholder_benefit_distribution, empirical, 'Whether selective disclosure distributes benefits to some external stakeholders or is purely unilateral extraction').

omega_variable(
    disclosure_mandate_enforceability,
    'Can transparency mandates be effectively enforced against institutional resistance, or do institutions maintain gatekeeping despite formal disclosure requirements?',
    'Measurement of compliance with mandatory disclosure timelines; analysis of redaction rates and scope across different regulatory regimes',
    'If enforceability strong: Scaffold perspective is realistic (sunset exists). If weak: Scaffold is aspirational and disclosure asymmetry persists despite formal mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disclosure_mandate_enforceability, empirical, 'Whether transparency mandates can enforce actual disclosure or institutions maintain gatekeeping despite requirements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(institutional_evidence_disclosure_asymmetry, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ieda_tr_t0, institutional_evidence_disclosure_asymmetry, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ieda_tr_t5, institutional_evidence_disclosure_asymmetry, theater_ratio, 5, 0.6).
narrative_ontology:measurement(ieda_tr_t10, institutional_evidence_disclosure_asymmetry, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ieda_tr_t15, institutional_evidence_disclosure_asymmetry, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(ieda_be_t0, institutional_evidence_disclosure_asymmetry, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ieda_be_t5, institutional_evidence_disclosure_asymmetry, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(ieda_be_t10, institutional_evidence_disclosure_asymmetry, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ieda_be_t15, institutional_evidence_disclosure_asymmetry, base_extractiveness, 15, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(institutional_evidence_disclosure_asymmetry, information_standard).
narrative_ontology:affects_constraint(institutional_evidence_disclosure_asymmetry, regulatory_capture).
narrative_ontology:affects_constraint(institutional_evidence_disclosure_asymmetry, epistemic_closure).
narrative_ontology:affects_constraint(institutional_evidence_disclosure_asymmetry, accountability_theater).

% DUAL FORMULATION NOTE:
% Institutional evidence disclosure asymmetry is upstream of regulatory capture (gatekeeping enables capture) and epistemic closure (asymmetry allows institutions to curate what counts as evidence). Downstream constraint accountability_theater depends on disclosure asymmetry for its performative mechanism. These constraints share a family structure: selective disclosure is the extraction mechanism that enables other institutional asymmetries.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(institutional_evidence_disclosure_asymmetry, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
