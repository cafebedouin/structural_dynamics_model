% ============================================================================
% CONSTRAINT STORY: gpl_reciprocity_obligation__copyleft_as_freedom_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_reciprocity_obligation__copyleft_as_freedom_reading, []).

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
 *   constraint_id: gpl_reciprocity_obligation__copyleft_as_freedom_reading
 *   human_readable: GPL Reciprocity Obligation (Copyleft-as-Freedom Reading)
 *   domain: software_licensing/intellectual_property/open_source_governance
 *
 * SUMMARY:
 *   The GPL reciprocity obligation — the requirement that derivative works
 *   remain open-source under GPL terms — is one of the most structurally
 *   analyzed constraints in open-source governance. This story instantiates
 *   the 'copyleft-as-freedom' reading: the constraint is understood as a
 *   mechanism to preserve downstream user freedoms by preventing proprietary
 *   capture of collectively developed code. The reciprocity obligation
 *   benefits downstream users (who gain permanent access to source and
 *   modification rights) while constraining proprietary integrators (who
 *   cannot closed-source GPL derivatives without violating the license). From
 *   the analytical perspective, this reading risks naturalizing the GPL as a
 *   necessary law of information freedom (if you want user freedom, you must
 *   enforce reciprocity). However, empirical data from permissive-license
 *   ecosystems suggests the freedom preservation function can also be
 *   achieved through community norms and reputational mechanisms with lower
 *   enforcement overhead. This story models the constraint as a genuine
 *   Tangled Rope from the integrated view: real coordination function
 *   (enabling code reuse and open development) layered with genuine
 *   extraction against proprietary business models (preventing closed-source
 *   derivatives). The suppression of proprietary options is structurally
 *   necessary to the freedom-preservation function from this reading's
 *   perspective. The temporal measurements show declining
 *   suppression_requirement (from 0.55 to 0.48) as permissive licensing norms
 *   strengthen, suggesting the constraint's enforcement load is naturally
 *   decreasing as the ecosystem matures — consistent with the Scaffold
 *   perspective's sunset logic.
 *
 * KEY AGENTS:
 *   - Downstream Users: Primary beneficiary (powerless/mobile) — gain permanent access to source code, modification rights, and assurance that derivative improvements remain open. Experiences constraint as pure coordination (Rope).
 *   - Proprietary Integrators: Primary victim (powerful/trapped) — cannot practically avoid GPL dependencies in infrastructure stack; forced to choose between opening proprietary code or abandoning GPL components. Experiences constraint as extraction (Snare from trapped perspective, Tangled Rope from constrained perspective).
 *   - Open Source Communities: Organized beneficiary (organized/constrained) — FSF, Linux Foundation, open-source projects maintaining GPL enforcement and governance structures. See reciprocity as temporary enforcement for ecosystem maturation with eventual sunset (Scaffold).
 *   - Permissive License Ecosystem: Institutional competitor (institutional/arbitrage) — MIT, Apache, BSD licenses achieving similar coordination with lower enforcement theater. GPL enforcement appears increasingly performative (Piton).
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing GPL reciprocity as inevitable law of information freedom when empirical evidence suggests community norms also suffice (False Summit Mountain).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.32).
domain_priors:suppression_score(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.48).
domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope).
narrative_ontology:human_readable(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "GPL Reciprocity Obligation (Copyleft-as-Freedom Reading)").
narrative_ontology:topic_domain(gpl_reciprocity_obligation__copyleft_as_freedom_reading, "software_licensing/intellectual_property/open_source_governance").

domain_priors:requires_active_enforcement(gpl_reciprocity_obligation__copyleft_as_freedom_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 'f4da63f6-ba71-4a8a-980e-2cc0df14723d').
narrative_ontology:cs_kernel_codification('f4da63f6-ba71-4a8a-980e-2cc0df14723d', formalized).
narrative_ontology:cs_authority_grounding('f4da63f6-ba71-4a8a-980e-2cc0df14723d', lineage).
narrative_ontology:cs_interpretation_layer_present('f4da63f6-ba71-4a8a-980e-2cc0df14723d').
narrative_ontology:cs_reading_relation('f4da63f6-ba71-4a8a-980e-2cc0df14723d', gpl_reciprocity_obligation__copyleft_as_restriction_reading, coexists_with).
narrative_ontology:cs_reading_relation('f4da63f6-ba71-4a8a-980e-2cc0df14723d', gpl_reciprocity_obligation__copyleft_as_commons_reading, coexists_with).
narrative_ontology:cs_axiom('f4da63f6-ba71-4a8a-980e-2cc0df14723d', foundational, reciprocity_preserves_downstream_freedom).
narrative_ontology:cs_axiom_status(reciprocity_preserves_downstream_freedom, holdable).
narrative_ontology:cs_axiom_grounding('f4da63f6-ba71-4a8a-980e-2cc0df14723d', reciprocity_preserves_downstream_freedom, deontological).
narrative_ontology:cs_axiom('f4da63f6-ba71-4a8a-980e-2cc0df14723d', foundational, proprietary_capture_threatens_freedom).
narrative_ontology:cs_axiom_status(proprietary_capture_threatens_freedom, holdable).
narrative_ontology:cs_axiom_grounding('f4da63f6-ba71-4a8a-980e-2cc0df14723d', proprietary_capture_threatens_freedom, empirically_contingent).
narrative_ontology:cs_reference_frame('f4da63f6-ba71-4a8a-980e-2cc0df14723d', copyleft_as_freedom_foundation).
narrative_ontology:cs_drift_state('f4da63f6-ba71-4a8a-980e-2cc0df14723d', contemporary_permissive_license_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f4da63f6-ba71-4a8a-980e-2cc0df14723d', '').
narrative_ontology:cs_kernel_id(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, downstream_users).
narrative_ontology:constraint_beneficiary(gpl_reciprocity_obligation__copyleft_as_freedom_reading, open_source_ecosystem).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_integrators).
narrative_ontology:constraint_victim(gpl_reciprocity_obligation__copyleft_as_freedom_reading, commercial_closed_source_vendors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM USER (ROPE) — Mobile exit options (can fork, modify, redistribute under GPL terms). Experiences the reciprocity obligation as pure coordination: the requirement that derivative works remain open preserves the user's own freedoms and prevents proprietary capture of their modifications. Beneficiary of the constraint — gains access to source code, modification rights, and assurance of future derivative availability.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: PROPRIETARY INTEGRATOR (TANGLED ROPE) — Constrained exit options (can use GPL code but cannot close-source derivative without violating license). Experiences mixed coordination and extraction: benefits from using GPL-licensed components (coordination function — code reuse, development cost reduction) but faces suppression of the proprietary business model (extraction — must open-source modifications or abandon the GPL component). Active enforcement required to maintain the reciprocity obligation against integrators attempting to circumvent it.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE COMMERCIAL VENDOR (SNARE) — Trapped by ubiquity of GPL components in infrastructure stack (Linux kernel, GNU tools, etc.). Cannot practically avoid GPL dependencies; exit is prohibitively costly. Experiences the reciprocal disclosure obligation as pure extraction with minimal coordination benefit — forced to choose between opening proprietary code or finding non-GPL alternatives (often unavailable). Experiences this constraint as coercive with high suppression of the proprietary business model.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (SCAFFOLD) — Organized agents (Free Software Foundation, Linux Foundation, open-source communities) maintaining GPL enforcement infrastructure with an implicit sunset: as open-source norms mature and proprietary software's economic value declines, the need for coercive reciprocity enforcement diminishes. The reciprocity obligation is temporary enforcement for a transitional period — enabling the open-source ecosystem to reach critical mass and legitimacy. Has sunset clause logic: as open-source becomes the default, proprietary capture becomes less viable and enforcement pressure relaxes.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PERMISSIVE LICENSE ECOSYSTEM (PITON) — MIT, Apache 2.0, and BSD licenses achieve similar coordination (code reuse, open development) with lower theater and lower enforcement overhead. The GPL's reciprocity rationale was necessary in the 1990s-2000s to prevent proprietary capture (Tivoization risk was real); permissive licenses now coexist with strong community and reputational norms against capture. GPL enforcement persists through institutional inertia (FSF, Linux Foundation governance structures) despite diminished empirical necessity. Theater ratio reflects that much GPL enforcement is performative — legal threat without substantial bite, as empirical proprietary capture of GPL code is rare in modern ecosystem.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal analytical perspective, copyleft represents an immutable property of information economies: any permissive licensing system will eventually be captured by proprietary actors unless reciprocal disclosure is mathematically enforced. This perspective naturalizes the GPL reciprocity obligation as a logical necessity (if you want downstream freedom, you must prevent upstream enclosure). However, empirical data contradicts this: permissive-license projects (Linux permissively licensed drivers, Apache-licensed Hadoop, MIT-licensed JavaScript frameworks) have resisted proprietary capture through community norms and reputational mechanisms alone. The mountain classification is a false summit — it naturalizes what is actually a historically contingent choice about enforcement strategy.
constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gpl_reciprocity_obligation__copyleft_as_freedom_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(gpl_reciprocity_obligation__copyleft_as_freedom_reading, TR),
    TR >= 0.70.

:- end_tests(gpl_reciprocity_obligation__copyleft_as_freedom_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Moderate-low. The GPL reciprocity obligation coordinates code reuse and open development (low-extraction coordination function) while suppressing proprietary business models (extraction against closed-source integrators). The net extractiveness is moderate because genuine coordination benefits exist alongside the suppression. This reading emphasizes the freedom-preservation (coordination) aspect over the proprietary-suppression (extraction) aspect, keeping extractiveness lower than the 'copyleft-as-restriction' reading would produce. Suppression (0.48): Moderate. The constraint suppresses proprietary business model options (inability to closed-source GPL derivatives), but this suppression is the intentional mechanism for preserving downstream freedoms from this reading's perspective. Not oppressive coercion but rather structural boundary maintenance. Theater ratio (0.35): Low-moderate. The reciprocity obligation has clear functional purpose (preventing proprietary capture) and generates measurable behavioral responses (compliance, license choice, architectural decisions). Less performative than traditional IP law, more tangible than pure theater. The theater ratio increases slightly over the interval as empirical proprietary capture becomes rare and enforcement becomes more norm-based than legal-threat-based, suggesting theater is rising relative to function.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the structural tension between this reading's frame and others. The downstream user sees Rope (pure coordination with no extraction experienced). The constrained proprietary integrator sees Tangled Rope (mixed benefit and suppression). The trapped large vendor sees Snare (pure extraction with no alternative). The open-source coalition sees Scaffold (temporary enforcement for ecosystem maturation). The permissive license ecosystem sees Piton (degraded GPL rationale, now-unnecessary enforcement persisting through inertia). The analytical observer risks seeing Mountain (copyleft as natural law) but empirical data about permissive-license resilience suggests this is a false summit. The core perspectival gap is between the freedom-preservation reading (this story) and the restriction-based reading: does the reciprocity obligation primarily enable downstream freedom, or primarily restrict upstream proprietary options? This story takes the first position; the sibling reading takes the second.
 *
 * DIRECTIONALITY LOGIC:
 *   The GPL reciprocity obligation's directionality depends on the agent's relationship to proprietary capture risk. Downstream users (beneficiaries of freedom preservation) have low d values — they experience the constraint as enabling rather than constraining. Proprietary integrators (victims of the suppression mechanism) have high d values — they experience suppression of their business model choices. The permissive license ecosystem represents a structural alternative with lower enforcement costs — their position relative to this constraint is one of arbitrage (they benefit from the freedom norm without the enforcement overhead). The analytical observer derives d from their position as an unaligned observer, taking a civilizational perspective on the constraint's necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the apparent contradiction between 'copyleft-as-freedom' and 'copyleft-as-restriction' readings reflects a genuine structural duality: the reciprocity obligation is simultaneously freedom-enabling (from the downstream user's perspective) and proprietary-suppressing (from the closed-source vendor's perspective). The constraint's Tangled Rope classification holds both aspects simultaneously. The mandatrophy dissolves when we recognize that the two readings are not competing empirical claims about the same phenomenon — they are different perspectival descriptions of the same structural mechanism. Both the freedom-preservation and the proprietary-restriction aspects are real. The analytical observer's mountain (copyleft-as-natural-law) is a false summit because it naturalizes what is actually a historically contingent choice: communities could preserve downstream freedom through permissive licensing + strong norms, which would lower suppression and theater while preserving the coordination function. The GPL reciprocity obligation is effective but not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proprietary_capture_counterfactual,
    'Would significant proprietary capture of GPL-licensed code have occurred without the reciprocal disclosure obligation, or would community norms and reputational mechanisms provide sufficient defense?',
    'Historical comparison: permissive-license ecosystems (Apache, MIT, BSD) vs GPL-licensed ecosystems; empirical rate of proprietary capture attempts and community response effectiveness; analysis of permissive-license projects that resisted capture (Linux permissive drivers, Hadoop, Node.js ecosystem)',
    'If capture would occur without GPL: reciprocity is functionally necessary for downstream user freedom (Snare or Tangled Rope from vendor perspective is justified). If community norms suffice: GPL reciprocity is contingent enforcement strategy, not natural law (Piton or Scaffold more accurate than Mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proprietary_capture_counterfactual, empirical, 'Whether proprietary capture is inevitable without GPL reciprocity or preventable through norms').

omega_variable(
    downstream_freedom_vs_upstream_constraint,
    'Does the GPL reciprocity obligation genuinely preserve downstream user freedoms, or does it primarily constrain upstream proprietary integrators'' choices?',
    'Empirical analysis of downstream user behavior: do downstream users perceive themselves as freer (more modification rights, more code access, more forking capability) under GPL vs permissive licenses? Survey of downstream user exit costs and perceived constraint. Compare with upstream integrator data on how reciprocity obligation constrains their technical choices and business models.',
    'If downstream freedom is primary: constraint is coordination mechanism (Rope from user perspective is correct). If upstream constraint is primary: constraint is extraction against proprietary interests (Snare from vendor perspective is correct). If both are salient: Tangled Rope classification confirmed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(downstream_freedom_vs_upstream_constraint, empirical, 'Whether GPL reciprocity primarily enables downstream freedom or constrains upstream proprietary business models').

omega_variable(
    enforcement_mechanism_dependency,
    'How much does the GPL reciprocity obligation''s effectiveness depend on active legal enforcement (FSF litigation, cease-and-desist) vs passive norm-based compliance (community pressure, reputational mechanisms)?',
    'Historical analysis of GPL enforcement actions and their outcomes; comparison of compliance rates in jurisdictions with strong vs weak IP enforcement; study of GPL circumvention attempts and community response; measurement of which GPL violations are caught and enforced vs which persist undetected',
    'If active enforcement is critical: constraint requires_active_enforcement=true is justified, Tangled Rope classification holds. If norm-based compliance dominates: enforcement is performative (Piton classification more accurate), theater_ratio should be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_dependency, empirical, 'Degree to which GPL reciprocity depends on active legal enforcement vs norm-based compliance').

omega_variable(
    reading_kernel_ambiguity,
    'Is the GPL reciprocity obligation grounded in a commitment to user freedom (this reading''s premise), in prevention of proprietary restrictions (restriction reading''s premise), or in maintenance of a commons (commons reading''s premise)? These premises may coexist or may be in tension.',
    'Textual analysis of GPL preamble and founding documents (Stallman''s GNU Manifesto); examination of FSF''s stated rationale in different periods; analysis of GPL enforcement priorities and arguments in litigation; study of how different GPL-variant communities (LGPL, AGPL, GPL3) have reframed the reciprocity obligation',
    'If user freedom is central: this reading''s axiom (reciprocity_preserves_downstream_freedom) is foundational and holdable. If preventing proprietary capture is central: restriction reading may be more foundational. If commons preservation is central: commons reading may be more foundational. Multiple coexisting rationales suggest ''coexists_with'' relation type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether GPL reciprocity is fundamentally about user freedom, proprietary prevention, or commons maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_freedom_tr_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gpl_freedom_tr_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(gpl_freedom_tr_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(gpl_freedom_be_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gpl_freedom_be_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gpl_freedom_be_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, base_extractiveness, 10, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(gpl_freedom_su_t0, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(gpl_freedom_su_t5, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 5, 0.52).
narrative_ontology:measurement(gpl_freedom_su_t10, gpl_reciprocity_obligation__copyleft_as_freedom_reading, suppression_requirement, 10, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_reciprocity_obligation__copyleft_as_freedom_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, 0.12).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_restriction_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, gpl_reciprocity_obligation__copyleft_as_commons_reading).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, permissive_licensing_ecosystem_adoption).
narrative_ontology:affects_constraint(gpl_reciprocity_obligation__copyleft_as_freedom_reading, proprietary_vendor_lock_in_prevention).

% DUAL FORMULATION NOTE:
% The GPL reciprocity obligation is a contested kernel with three structurally distinct readings, each producing a different constraint story with different ε values, beneficiary/victim structures, and classification patterns. This story (copyleft-as-freedom, ε=0.32, Tangled Rope) emphasizes the downstream-freedom coordination function. The copyleft-as-restriction reading (ε higher, more Snare-oriented) emphasizes proprietary suppression. The copyleft-as-commons reading (ε lower, more Rope-oriented) emphasizes resource governance. All three share the same kernel (the reciprocal-disclosure requirement) but interpret its purpose, function, and legitimacy differently. The three readings are linked as siblings via cs_structure.reading_relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gpl_reciprocity_obligation__copyleft_as_freedom_reading, powerful, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
