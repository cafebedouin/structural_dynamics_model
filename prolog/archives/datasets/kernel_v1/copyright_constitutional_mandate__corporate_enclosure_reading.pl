% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Corporate Enclosure: Constitutional Mandate Reading
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   The corporate enclosure reading interprets the Copyright Clause
 *   ('Congress shall have Power... To promote the Progress of Science and
 *   useful Arts, by securing for limited Times to Authors... exclusive Right
 *   to their respective Writings') as mandating maximal intellectual-property
 *   protection for corporate rights holders, with 'limited times' understood
 *   as a formal distinction from perpetuity compatible with indefinitely
 *   extended terms. This reading has dominated U.S. copyright law since the
 *   1976 Copyright Act and DMCA 1998. Under this reading, copyright is
 *   constitutionally treated as a property right requiring maximum
 *   enforcement: term extensions (1976→1998→ongoing proposals for further
 *   extension), criminalization of technical circumvention (DMCA § 1201), and
 *   restrictive interpretation of fair-use exceptions. The constraint
 *   benefits corporate media incumbents (Disney, RIAA, MPAA) who extract
 *   rents from back-catalogue licensing and rights control, while victimizing
 *   derivative creators, educators, archivists, and the broader cultural
 *   commons. The corporate enclosure reading competes with the
 *   public-scaffold reading (which treats copyright as a temporary incentive
 *   mechanism designed to create public-domain entry and knowledge commons)
 *   and the judicial-ambiguity reading (which sees courts formally
 *   maintaining fair use while substantively narrowing it through high
 *   evidentiary bars and statutory-damages risk). This story instantiates the
 *   corporate enclosure reading only — the other readings are separate
 *   constraint stories with different ε values and victim/beneficiary
 *   structures.
 *
 * KEY AGENTS:
 *   - Corporate Media Incumbents (Disney, RIAA, MPAA, Time Warner): Primary beneficiary (institutional/arbitrage) — capture rents from back-catalogue licensing, term extension, DMCA § 1201 lock-in. Experience constraint as coordinating access to fragmented rights markets.
 *   - Derivative Creators (remixers, samplers, fan-fiction writers, adaptive artists): Primary victim (moderate/constrained) — face licensing barriers and legal risk under DMCA § 1201. Cannot build on existing cultural works at scale.
 *   - Educators and Libraries (universities, public libraries, archives): Secondary victim (moderate/constrained) — face licensing costs and fair-use narrowing. Cannot teach media literacy using copyrighted materials without risk.
 *   - Archivists and Historians: Secondary victim (powerless/trapped) — cannot digitize and preserve cultural heritage without indefinite licensing obligations. Copyright prevents time-shifting preservation of works.
 *   - Copyright Office and Courts: Institutional actors (institutional/arbitrage) — maintain formal doctrine (fair use exists, public domain exists) while narrowing enforcement. Theater increasing as doctrine becomes performative.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks seeing copyright maximalismism as a natural law ('intellectual property is property') rather than a constructed constitutional reading designed to extract rents.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.68).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.75).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, snare).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Corporate Enclosure: Constitutional Mandate Reading").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '5bf3c965-db75-4ecb-a1dc-bb77a8f26275').
narrative_ontology:cs_kernel_codification('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', fixed_text).
narrative_ontology:cs_authority_grounding('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', lineage).
narrative_ontology:cs_interpretation_layer_present('5bf3c965-db75-4ecb-a1dc-bb77a8f26275').
narrative_ontology:cs_reading_relation('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', copyright_constitutional_mandate__public_scaffold_reading, coexists_with).
narrative_ontology:cs_reading_relation('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', foundational, copyright_is_property_right_requiring_maximum_protection).
narrative_ontology:cs_axiom_status(copyright_is_property_right_requiring_maximum_protection, holdable).
narrative_ontology:cs_axiom_grounding('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', copyright_is_property_right_requiring_maximum_protection, conventional).
narrative_ontology:cs_axiom('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', foundational, limited_times_permits_indefinite_extension_short_of_perpetuity).
narrative_ontology:cs_axiom_status(limited_times_permits_indefinite_extension_short_of_perpetuity, holdable).
narrative_ontology:cs_axiom_grounding('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', limited_times_permits_indefinite_extension_short_of_perpetuity, conventional).
narrative_ontology:cs_reference_frame('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', copyright_as_perpetual_property_right_framework).
narrative_ontology:cs_drift_state('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', contemporary_corporate_maximalist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5bf3c965-db75-4ecb-a1dc-bb77a8f26275', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, corporate_media_incumbents).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, entertainment_rights_holders).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators_and_libraries).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists_and_historians).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EDUCATOR/ARCHIVIST (SNARE) — Faces absolute prohibition on fair-use adaptation of copyrighted materials under the corporate enclosure reading. Cannot teach media literacy using film clips, cannot archive cultural heritage without licensing fees, cannot enable derivative scholarship. Exit is material impossibility: the constraint applies globally and violates no statutory exception.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT CREATOR (SNARE) — Cannot build on existing cultural works (sampling, remix, fan fiction, adaptation) without negotiating with rights holders who have zero incentive to license. DMCA § 1201 circumvention liability makes reverse-engineering of technical protection measures a criminal act, even for interoperable creation. High cost and legal risk suppress derivative creativity at scale.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: CORPORATE MEDIA INCUMBENT (ROPE) — Experiences the constraint as coordination infrastructure protecting long-tail revenue streams and back-catalogue licensing. Term extension enables licensing of decades-old content; DMCA § 1201 prevents unauthorized circumvention that would cannibalize licensing fees. The constraint solves a genuine coordination problem (many rights holders, complex licensing chains) while generating extractive rents. Net beneficiary.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: COPYRIGHT OFFICE & JUDICIARY (PITON) — The office maintains increasingly performative classification of 'useful article' exceptions and fair-use boundaries. Courts narrowly construe fair use (Sony v. Universal, Harper & Row v. Nation) and broadly interpret statutory damages to deter circumvention litigation. The institutional machinery performs robust protection while the underlying legal boundaries have eroded. Theater maintained through interpretive narrowing and statutory maximalism.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, copyright is a natural law of authorial compensation: creators naturally 'own' their output, and maximal protection is simply recognizing this inherent property. The corporate enclosure reading naturalizes this as immutable constitutional mandate. However, the beneficiary and victim declarations reveal this as a false summit — the constraint benefits identifiable corporate actors whose interests drive legislative maximalism. The 'natural' framing conceals the constructed extraction mechanism.
constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_constitutional_mandate__corporate_enclosure_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, TR),
    TR >= 0.70.

:- end_tests(copyright_constitutional_mandate__corporate_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, approaching snare threshold. The constraint extracts significant rents through: (1) term extension preventing public-domain entry (Disney's Mickey Mouse kept in copyright through successive term extensions; Sonny Bono Act extended works already published by 20 years); (2) DMCA § 1201 criminalizing circumvention and enabling lock-in (e-books bound to proprietary readers, streaming content bound to subscription services); (3) statutory damages ($750–$30k per work, $150k for willful infringement) creating asymmetric litigation risk; (4) narrow fair-use interpretation requiring case-by-case licensing negotiation for derivative works. These mechanisms transfer wealth from creators and users to rights holders with no increase in creation incentives (empirical studies show zero marginal effect of term extension on authorial output beyond 14-year baseline). Suppression (0.75): High. Multiple enforcement vectors create redundant suppression: copyright law (private civil action), DMCA § 1201 (criminal liability for circumvention), contract law (terms of service overriding copyright balance), technological control (DRM systems), and litigation risk (statutory damages exceed actual harm). The suppression is increasing over the interval: Sony Bono Act (1998) and DMCA (1998) vastly expanded enforcement capacity. Theater ratio (0.58): Moderate-high. Fair use exists formally but is narrowed through interpretive narrowing (high bar for 'transformativeness,' reduction of market harm to irrelevance, statutory damages as threat). The Copyright Office maintains 'useful article' and fair-use categories performatively — they exist on paper but rarely succeed in litigation. DMCA § 1201 is maximally performative: the statute criminalizes circumvention regardless of whether the circumvented content is itself copyrighted (enabling lock-in on public-domain content).
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces a stark perspectival gap between corporate beneficiaries (Rope perspective: experiencing coordination benefits and licensing optionality) and trapped victims (Snare perspective: facing absolute prohibition on fair use, archival preservation, interoperable creation). Independent creators in the constrained-exit category see snare classification — they could theoretically negotiate licenses but face asymmetric bargaining power and astronomical fees. The Copyright Office and courts maintain a Piton perspective: they perform robust copyright protection while the underlying legal distinctions (fair use, public domain, useful article) have eroded to near-irrelevance. The analytical observer's Mountain perspective naturalizes copyright as inherent property right, but the beneficiary and victim declarations reveal this as a false summit — the constraint's protection scales with corporate lobbying capacity, not with authorial creation incentives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural relationship to the extraction flow. Corporate incumbents are beneficiaries with arbitrage options (low d → negative f(d) → subsidized by constraint). Derivative creators are moderate-power victims with constrained exit (medium-high d → moderate f(d) → significant extraction). Educators and archivists are moderate-power victims with constrained exit (medium-high d → moderate f(d)). The powerless archivists facing absolute prohibition on preservation without licensing are trapped victims (high d → high f(d) → maximal extraction). The Copyright Office performs institutional arbitrage (low d initially, but increasing institutional lock-in reduces exit optionality, approaching constrained). The analytical observer's naturalization of copyright as property creates identity_locked dynamics — the observer cannot see the constraint as constructed because their professional identity is fused with the IP-as-property paradigm.
 *
 * MANDATROPHY ANALYSIS:
 *   The corporate enclosure reading resolves mandatrophy by declaring its beneficiaries and victims transparently: this is a snare from the perspective of constrained and trapped agents, a rope from the perspective of corporate beneficiaries. The constraint is not mandatrophic — it is unambiguously extractive for most observing positions. The mandatrophy arises at the kernel level: the Copyright Clause could plausibly be read as mandating public-scaffold logic (limited terms for incentive optimization) or as compatible with corporate-enclosure maximalism. The corporate enclosure reading resolves the constitutional ambiguity by choosing the property-rights framing. The public-scaffold reading would resolve it by choosing the incentive-optimization framing. Both readings are live constitutional positions held by different legal coalitions (incumbents vs. digital-commons advocates). Neither reading forecloses the other within the framework of constitutional interpretation — both are defensible readings of 'limited times' and 'promote Progress.' They coexist as competing constitutional claims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    limited_times_construction,
    'Does ''limited times'' in the Copyright Clause mean a temporal limit that encourages creation (public-scaffold reading) or merely a formal distinction from perpetuity compatible with maximally extended terms (corporate-enclosure reading)?',
    'Historical analysis of founding-era language, Constitutional Convention debates, and early congressional interpretation of term duration. Correlation with evidence of actual creation incentives at different term lengths (empirical elasticity studies of author output vs. copyright term).',
    'If ''limited times'' constrains Congress toward shorter terms for incentive purposes: public-scaffold reading''s axiom is holdable. If ''limited times'' is merely a formal gate permitting indefinite legislative extensions: corporate-enclosure reading''s axiom (maximal extension within formalism) is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(limited_times_construction, empirical, 'Interpretation of ''limited times'' clause in Copyright Clause').

omega_variable(
    fair_use_constitutional_baseline,
    'Is fair use a constitutionally mandated exception to copyright (part of the Clause''s internal balance), or a statutory grace note Congress can narrow arbitrarily?',
    'Jurisprudential analysis of fair use as First Amendment right vs. statutory privilege. Look for judicial language treating fair use as constitutionally required vs. congressionally granted. Empirical: correlation between fair-use narrowing (Sony, Harper & Row, Access Copyright decisions) and claimed copyright maximalism.',
    'If fair use is constitutional: corporate enclosure reading forecloses itself (cannot maintain maximal extraction while respecting constitutional baseline). If fair use is statutory privilege: corporate enclosure reading remains coherent (Congress can withdraw it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_constitutional_baseline, conceptual, 'Constitutional status of fair use exception').

omega_variable(
    incentive_justification_threshold,
    'At what copyright term length does the empirical evidence show zero marginal incentive effect for additional term extension? Is copyright presently at, above, or below that threshold?',
    'Systematic review of authorial-output studies (Landes & Posner, Boldrin & Levine, Mulligan & Salanie, Rufus Pollock econometric analyses). Meta-analysis of elasticity of creation with respect to copyright term across media (music, film, literature, software).',
    'If presently above the threshold: term extension is pure extraction (no public-domain creation incentive). Corporate enclosure reading is empirically maximalist. If below: corporate enclosure conflicts with its own justificatory apparatus (copyright theoretically designed to incentivize creation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incentive_justification_threshold, empirical, 'Marginal incentive effect of copyright term extension').

omega_variable(
    dmca_circumvention_statutory_scope,
    'Does DMCA § 1201 circumvention liability apply to interoperable creation (reverse engineering for compatibility, not infringement), and if so, what is the statutory justification?',
    'Textual analysis of § 1201 and legislative record. Empirical: count of § 1201 enforcement actions; fraction targeting interoperable creation vs. direct piracy. Court interpretation of ''circumvention'' in Sony v. Interconnect Systems, Chamberlain v. Skylink, and subsequent decisions.',
    'If § 1201 routinely applies to interoperable creation: suppression is higher than base extraction suggests (technical locks become enforcement mechanism beyond copyright scope). If § 1201 enforcement is narrowly targeted: suppression is lower (legal tools exist to distinguish interoperable creation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dmca_circumvention_statutory_scope, empirical, 'DMCA § 1201 scope and enforcement patterns').

omega_variable(
    kernel_reading_contest,
    'Which reading of the copyright constitutional mandate (corporate enclosure vs. public scaffold vs. judicial ambiguity) represents the operative legal framework that courts and Congress currently instantiate?',
    'Jurisprudential analysis of judicial doctrine. Legislative voting patterns on term extension (Sony Bono Act, copyright term-extension renewal, DMCA passage). Regulatory agency interpretation (Copyright Office fair-use studies, DMCA rulemaking).',
    'If corporate enclosure reading is dominant: the constraint is a snare from most perspectives. If public scaffold reading is dominant: the constraint is tangled rope with sunset. If judicial ambiguity reading is dominant: the constraint is piton (courts narrowly construe fair use while maintaining formal copyright framework).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, empirical, 'Which copyright constitutional reading is presently operative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyrenc_tr_t0, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(copyrenc_tr_t10, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 10, 0.55).
narrative_ontology:measurement(copyrenc_tr_t20, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(copyrenc_be_t0, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(copyrenc_be_t5, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(copyrenc_be_t10, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(copyrenc_be_t15, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(copyrenc_be_t20, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(copyrenc_su_t0, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(copyrenc_su_t10, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(copyrenc_su_t20, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.18).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate__judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_circumvention_liability).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_narrowing).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, cultural_commons_accessibility).

% DUAL FORMULATION NOTE:
% The copyright constitutional mandate is a contested kernel with three structurally distinct readings: corporate enclosure (this story, ε=0.68, snare), public scaffold (separate story, lower ε, tangled rope with sunset), and judicial ambiguity (separate story, piton with theater maintenance). Each reading instantiates different beneficiary/victim structures, different ε values, and different terminal classifications. The three stories share the same kernel (the Copyright Clause) but diverge in interpretation. Downstream constraints (DMCA § 1201, fair-use narrowing, cultural commons accessibility) are affected by which reading becomes operationally dominant in law and policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_constitutional_mandate__corporate_enclosure_reading, analytical, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
