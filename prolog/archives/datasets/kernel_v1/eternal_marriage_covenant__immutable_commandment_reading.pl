% ============================================================================
% CONSTRAINT STORY: eternal_marriage_covenant__immutable_commandment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eternal_marriage_covenant__immutable_commandment_reading, []).

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
 *   constraint_id: eternal_marriage_covenant__immutable_commandment_reading
 *   human_readable: Eternal Marriage Covenant as Immutable Commandment (D&C 132)
 *   domain: religious_law/political_theology/commitment_system
 *
 * SUMMARY:
 *   D&C 132 establishes eternal marriage as a divine commandment required for
 *   the highest level of exaltation. The immutable commandment reading
 *   interprets this as a metaphysical law of the cosmos — unchangeable, not
 *   subject to human revision, binding in all eternities. Federal prosecution
 *   of polygamy (1862-1890) and subsequent internal pressure created a
 *   situation where this reading demands that the faithful choose between
 *   federal law compliance and doctrinal obedience. The 1890 Manifesto
 *   technically 'suspended' polygamy practice while leaving the theological
 *   command intact in official canon. This creates an ambiguity at the kernel
 *   level: is D&C 132 (a) an immutable law of creation that is temporarily
 *   unenforced, (b) a covenant that can be revised or suspended by prophetic
 *   authority, or (c) a doctrine that was always meant to be accommodated to
 *   historical circumstances? The immutable commandment reading forecloses
 *   the accommodation reading but coexists with the override reading — both
 *   cannot be simultaneously true within a single institutional framework,
 *   but different factions hold each view. The immutable reading creates
 *   maximal extraction for those who experience it as binding (female
 *   members, non-plural spouses) and maximal benefit for institutional
 *   authority (which gains control over doctrinal revision, gender hierarchy,
 *   and reproductive incentives).
 *
 * KEY AGENTS:
 *   - Institutional Church Authority (LDS leadership): Primary beneficiary (institutional/arbitrage) — maintains doctrinal authority, prevents progressive revision, preserves gender hierarchy. Experiences the immutable framing as coordination (holds the institution together against schism and secular pressure).
 *   - Female Members (especially in polygamous contexts): Primary victim (powerless/identity_locked) — faces choice between covenant obedience (accepting polygamy as eternal) and apostasy. Identity fused with the religious framework; structural mobility but cognitive/spiritual entrapment.
 *   - Non-Plural Spouses: Secondary victim (powerless/constrained or identity_locked) — faces spousal addition as potential eternal necessity; experiences the immutability claim as removal of exit options.
 *   - Doctrinal Dissenters (intellectually aware members): Secondary victim (moderate/constrained) — sees the inconsistencies (Manifesto as precedent, female agency bracketing, covenant-vs-law ambiguity) but faces career, family, and community costs of public dissent.
 *   - Federal Government (1862-1890): External actor (powerful/mobile) — prosecutes polygamy, creates pressure that intensifies the immutability claim as institutional defense.
 *   - Analytical Observer: Detached position (analytical/analytical) — risks naturalizing a contingent institutional commitment as cosmic law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, 0.68).
domain_priors:suppression_score(eternal_marriage_covenant__immutable_commandment_reading, 0.78).
domain_priors:theater_ratio(eternal_marriage_covenant__immutable_commandment_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eternal_marriage_covenant__immutable_commandment_reading, snare).
narrative_ontology:human_readable(eternal_marriage_covenant__immutable_commandment_reading, "Eternal Marriage Covenant as Immutable Commandment (D&C 132)").
narrative_ontology:topic_domain(eternal_marriage_covenant__immutable_commandment_reading, "religious_law/political_theology/commitment_system").

domain_priors:requires_active_enforcement(eternal_marriage_covenant__immutable_commandment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eternal_marriage_covenant__immutable_commandment_reading, 'a47e8e8c-78c7-4f68-b16e-78829c4560ff').
narrative_ontology:cs_kernel_codification('a47e8e8c-78c7-4f68-b16e-78829c4560ff', fixed_text).
narrative_ontology:cs_authority_grounding('a47e8e8c-78c7-4f68-b16e-78829c4560ff', extraction).
narrative_ontology:cs_interpretation_layer_present('a47e8e8c-78c7-4f68-b16e-78829c4560ff').
narrative_ontology:cs_reading_relation('a47e8e8c-78c7-4f68-b16e-78829c4560ff', eternal_marriage_covenant__prophetic_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('a47e8e8c-78c7-4f68-b16e-78829c4560ff', eternal_marriage_covenant__temporal_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('a47e8e8c-78c7-4f68-b16e-78829c4560ff', foundational, eternal_marriage_metaphysical_immutability).
narrative_ontology:cs_axiom_status(eternal_marriage_metaphysical_immutability, holdable).
narrative_ontology:cs_axiom_grounding('a47e8e8c-78c7-4f68-b16e-78829c4560ff', eternal_marriage_metaphysical_immutability, deontological).
narrative_ontology:cs_axiom('a47e8e8c-78c7-4f68-b16e-78829c4560ff', secondary, gender_hierarchy_cosmic_order).
narrative_ontology:cs_axiom_status(gender_hierarchy_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('a47e8e8c-78c7-4f68-b16e-78829c4560ff', gender_hierarchy_cosmic_order, deontological).
narrative_ontology:cs_reference_frame('a47e8e8c-78c7-4f68-b16e-78829c4560ff', eternal_covenant_always_binding).
narrative_ontology:cs_drift_state('a47e8e8c-78c7-4f68-b16e-78829c4560ff', contemporary_post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a47e8e8c-78c7-4f68-b16e-78829c4560ff', '').
narrative_ontology:cs_kernel_id(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eternal_marriage_covenant__immutable_commandment_reading, institutional_church_authority).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, female_members).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, non_plural_spouses).
narrative_ontology:constraint_victim(eternal_marriage_covenant__immutable_commandment_reading, doctrinal_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBEDIENT FEMALE MEMBER (SNARE) — Structurally mobile (could physically leave) but identity-fused with the religious framework. Her self-concept, family bonds, eternal salvation narrative, and community standing are all constituted through the covenant. Compliance means accepting polygamy as divine will; exit would require abandoning not just a marriage but her identity. Maximum extraction — she internalizes the constraint as spiritual duty.
constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 2: DISSENTING MEMBER / INTELLECTUAL CONFLICT (TANGLED ROPE) — Sees the coordination function (eternal family structure, gender hierarchy codification, reproductive incentives) but also sees the asymmetric extraction (women's reproductive autonomy, spousal equality, doctrinal rigidity). Career, family ties, and community status constrain exit; the framework also provides genuine community goods (support networks, ritual meaning, identity continuity). Mixed experience.
constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSTITUTIONAL AUTHORITY STRUCTURE (ROPE) — The church leadership experiences the immutable commandment framing as a coordination mechanism: it secures doctrinal authority, prevents progressive revision, ensures membership compliance through theological inertia, and maintains the gender/kinship hierarchy that concentrates power in male institutional roles. The framework solves the problem of maintaining institutional control in the face of external (federal) and internal (feminist) pressure. Net beneficiary experiencing this as pure coordination.
constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER / NATURAL LAW FRAMING (MOUNTAIN) — From an analytical distance, the immutable commandment reading presents itself as a statement of eternal metaphysical law: the structure of the cosmos, divine governance, human nature, and gender roles are presented as unchangeable. This perspective risks naturalizing what is actually a contingent institutional and political commitment. The false summit signature detects that identifiable beneficiaries exist — institutional authority benefits from the immutability claim — suggesting the naturalization is strategic rather than necessary.
constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eternal_marriage_covenant__immutable_commandment_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eternal_marriage_covenant__immutable_commandment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(eternal_marriage_covenant__immutable_commandment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The immutable commandment reading extracts reproductive autonomy from women (potential requirement to accept plural marriage), doctrinal autonomy from members (no legitimate path to question the covenant), and institutional autonomy from the organization (no revision possible). The extraction is substantial but not maximal (0.85+) because some members willingly accept the framework, and the institutional structure provides genuine community goods (ritual meaning, identity, support networks). The measurement trajectory shows rising extractiveness over the 100-year interval (0.52 → 0.68), reflecting the accumulation of enforcement infrastructure and the intensification of the immutability rhetoric as external and internal pressure increases. Suppression (0.78): High. The immutable reading is maintained through identity fusion (internalized belief that revision is sin), institutional enforcement (selective doctrine emphasis, authority claims to prophetic insight), and external legal consequence (federal prosecution historically, ongoing legal marginalization of fundamentalist splinter groups). Suppression rises over the interval (0.65 → 0.78) as the institutional apparatus for maintaining doctrinal conformity strengthens and identity-locked mechanisms deepen. Theater ratio (0.55): Moderate. The immutable commandment framing involves performative elements — ceremonial affirmation of eternal marriage covenants, ritual repetition that brackets questions about actual consent and compliance — but also contains genuine functional content: the framework does coordinate family structures, reproductive incentives, and gender hierarchy. The ratio rises modestly (0.38 → 0.55) as the Manifesto creates a gap between theological teaching (immutable) and actual practice (suspended), forcing more performative language to maintain the contradiction.
 *
 * PERSPECTIVAL GAP:
 *   The immutable commandment reading produces maximum perspectival divergence. Female members experience it as a snare (identity-locked extraction, no exit). Institutional authority experiences it as rope (pure coordination). Dissenters experience it as tangled rope (mixed coordination and extraction). The analytical observer risks seeing it as a mountain (natural law). The gap reveals the structural asymmetry: the immutability claim benefits the beneficiary (institutional authority) by preventing revision, while it harms the victim (female members) by removing the exit option of doctrinal appeal. The false summit signature fires: the mountain classification is a naturalization of a contingent political and institutional commitment, revealed by the presence of identifiable beneficiaries (institutional authority) who benefit from the immutability claim itself.
 *
 * DIRECTIONALITY LOGIC:
 *   The immutable commandment reading assigns directionality based on structural relationship to the extraction flow. Female members identified as victims with identity_locked exit derive high d (≈0.85-0.90), producing high f(d) ≈ 1.2, making their experienced extractiveness χ substantial. Institutional authority identified as beneficiary with arbitrage exit derives low d (≈0.10-0.15), producing low f(d) ≈ -0.05, making their experienced extractiveness negative (the constraint extracts toward them, not from them). Doctrinal dissenters with constrained exit and mixed victim/beneficiary status derive moderate d (≈0.55-0.65), producing moderate f(d) ≈ 0.75. The institutional perspective on the constraint (rope) reflects that the authority structure genuinely solves a coordination problem (holding the institution together, preserving gender hierarchy, maintaining doctrinal control) while simultaneously extracting from those it governs.
 *
 * MANDATROPHY ANALYSIS:
 *   The immutable commandment reading resolves mandatrophy by showing that the classification asymmetry is NOT an indeterminacy but a structural fact. The beneficiary (institutional authority) experiences rope (coordination); the victim (female members) experiences snare (extraction); the analytical observer risks seeing mountain (natural law). These are not competing 'views' of the same underlying phenomenon — they are structurally different experiences of the same constraint. Mandatrophy is resolved by recognizing that the immutability claim is maintained because it benefits institutional authority, and that the constraint would be classified differently (as revisable, as covenant-conditional, as temporal accommodation) if the beneficiary/victim relationship were inverted or if alternative readings were institutionally privileged.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    canonical_immutability_grounding,
    'Is D&C 132''s immutability claim grounded in revealed metaphysical fact or in institutional authority''s need to prevent doctrinal revision?',
    'Comparative textual analysis of immutability claims across LDS canon; historical analysis of when and how immutability language was emphasized (especially post-Manifesto periods); analysis of whether immutability claims correlate with external pressure (federal polygamy prosecution, internal dissent)',
    'If grounded in metaphysical revelation: the immutable commandment reading is legitimate doctrinal closure. If grounded in institutional control: the reading is a strategic naturalization, and alternative readings (prophetic override, temporal accommodation) become structurally visible as suppressed but viable positions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(canonical_immutability_grounding, empirical, 'Whether immutability claim rests on revealed metaphysics or institutional authority').

omega_variable(
    female_agency_bracketing,
    'How does the immutable commandment reading account for female members'' explicit consent and spiritual agency in the covenant?',
    'Analysis of canonical teaching about women''s role in accepting the covenant; comparison with historical records of women''s actual choices and resistance; assessment of whether ''spiritual consent'' can be distinguished from coerced compliance within the identity-locked frame',
    'If female agency is structural (not bracketed): snare classification may be overstated; mixed experience (tangled_rope) may better fit. If agency is bracketed as already-determined by the covenant: snare is confirmed as the target agent''s structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(female_agency_bracketing, empirical, 'Whether female agency in covenant acceptance is genuinely operative or formally bracketed').

omega_variable(
    manifesto_as_precedent,
    'Does the 1890 Manifesto — which suspended the polygamy commandment under federal pressure — constitute a precedent for prophetic revision of immutable commands, or is it framed as a temporary accommodation that leaves the command immutable in principle?',
    'Canonical analysis of how the Manifesto is explained in doctrine; comparison of immutability language before and after 1890; assessment of whether official teaching frames the Manifesto as a true revision or as a suspended-but-eternal obligation',
    'If Manifesto is precedent for revision: the immutable commandment reading is undermined by its own institutional history, and the temporal accommodation or prophetic override readings become visible. If Manifesto is framed as suspension-not-revision: the immutable commandment reading is confirmed, and the readings become logically foreclosed alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manifesto_as_precedent, conceptual, 'Whether 1890 Manifesto establishes precedent for revising immutable commands').

omega_variable(
    covenant_vs_law_ambiguity,
    'Is eternal marriage a timeless law of the cosmos, or a covenant (a mutual agreement binding specific parties under specified conditions)?',
    'Textual analysis distinguishing covenant language (bilateral, conditional, between specific parties) from law language (universal, unconditional, binding all); analysis of how immutability applies differently to laws (unchanging) vs covenants (changeable through mutual agreement or divine revision)',
    'If eternal marriage is primarily covenant: the immutable reading conflates the covenant''s existence with its immutability — covenants can persist while their enforcement becomes conditional or waived. If primarily law: immutability is coherent. The grammatical and doctrinal distinction is the crux of whether the readings coexist or foreclose.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(covenant_vs_law_ambiguity, conceptual, 'Semantic ambiguity between immutable law and revisable covenant').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.78) structural (institutional barriers, legal consequences, family separation) or internalized (belief that deviation is sin, identity as covenant-keeper)?',
    'Post-exit trajectory analysis: do members who leave the LDS institutional structure retain suppression effects (belief they violated eternal law, fear of cosmic consequences, identity crisis) after external barriers are removed? Longitudinal psychological assessment of identity_locked vs constrained vs trapped mechanisms.',
    'If primarily internalized: the constraint''s suppressive force travels with the agent and persists after institutional exit, making the snare classification stable. If primarily structural: suppression would decline post-exit, suggesting the trap is institutional + identity fusion rather than pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural institutional barriers or internalized belief').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eternal_marriage_covenant__immutable_commandment_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emc_immutable_theater_t0, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(emc_immutable_theater_t50, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(emc_immutable_theater_t100, eternal_marriage_covenant__immutable_commandment_reading, theater_ratio, 100, 0.55).

% Extraction over time
narrative_ontology:measurement(emc_immutable_extractiveness_t0, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(emc_immutable_extractiveness_t50, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(emc_immutable_extractiveness_t100, eternal_marriage_covenant__immutable_commandment_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(emc_immutable_suppression_t0, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(emc_immutable_suppression_t50, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 50, 0.72).
narrative_ontology:measurement(emc_immutable_suppression_t100, eternal_marriage_covenant__immutable_commandment_reading, suppression_requirement, 100, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eternal_marriage_covenant__immutable_commandment_reading, identity_coordination).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__prophetic_override_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, eternal_marriage_covenant__temporal_accommodation_reading).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, federal_polygamy_prosecution).
narrative_ontology:affects_constraint(eternal_marriage_covenant__immutable_commandment_reading, fundamentalist_splinter_identity_lock).

% DUAL FORMULATION NOTE:
% The eternal_marriage_covenant kernel contains three structurally distinct constraints with different ε values, corresponding to the three readings. The immutable commandment reading (this story) has ε ≈ 0.68 (high extraction, high suppression). The prophetic override reading has lower ε (≈0.45, tangled rope) because it preserves institutional authority while acknowledging doctrinal revisability. The temporal accommodation reading has lowest ε (≈0.25, rope or scaffold) because it treats the commandment as contextually binding, not eternally immutable. Each reading is a separate constraint; the three readings are linked through this network edge showing they compete to define the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
