% ============================================================================
% CONSTRAINT STORY: copyright_constitutional_mandate__corporate_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
 *   constraint_id: copyright_constitutional_mandate__corporate_enclosure_reading
 *   human_readable: Copyright as Maximal-Protection Property Right (Corporate Enclosure Reading)
 *   domain: intellectual_property_law/constitutional_law/political_economy
 *
 * SUMMARY:
 *   This constraint instantiates the corporate-enclosure reading of the
 *   copyright constitutional mandate kernel: the claim that copyright is
 *   fundamentally a property right deserving maximal protection, and that the
 *   constitutional phrase 'limited times' should be read as permitting
 *   maximal extension short of an explicit, formally perpetual term. Under
 *   this reading, each term extension (1976, 1998) is not a departure from
 *   the constitutional bargain but a correction toward the level of
 *   protection property rights inherently deserve. The reading is authored
 *   here as a clean, ε-invariant constraint: it does not average over or
 *   hedge against the sibling readings (public_scaffold_reading,
 *   judicial_ambiguity_reading), which are separate constraint stories with
 *   their own ε values and their own beneficiary/victim structures. Under
 *   this reading's own lights, the standing arrangement — serial term
 *   extension, DMCA anti-circumvention criminalization, and fair-use
 *   narrowing — is high-extraction: it transfers control from the public and
 *   downstream creators to concentrated corporate rightsholders while the
 *   incentive rationale that justifies the initial grant does not extend to
 *   already-created works.
 *
 * KEY AGENTS:
 *   - legacy_entertainment_conglomerates: primary beneficiary and agenda-setter (institutional/arbitrage) — captures licensing value from extended terms
 *   - major_record_labels, film_studio_licensing_arms: secondary institutional beneficiaries with catalog-timed lobbying incentives
 *   - derivative_creators, educators, archivists, documentary_filmmakers: primary targets bearing the extraction — trapped or constrained exit
 *   - public_domain_researchers: excluded analytical voice with empirical counter-evidence not reaching legislative process
 *   - congress: agenda-setter that enacted extension legislation under lobbying pressure
 *   - federal_courts: observer seat, validated extensions under rational-basis deference
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81).
domain_priors:suppression_score(copyright_constitutional_mandate__corporate_enclosure_reading, 0.72).
domain_priors:theater_ratio(copyright_constitutional_mandate__corporate_enclosure_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(copyright_constitutional_mandate__corporate_enclosure_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_constitutional_mandate__corporate_enclosure_reading, tangled_rope).
narrative_ontology:human_readable(copyright_constitutional_mandate__corporate_enclosure_reading, "Copyright as Maximal-Protection Property Right (Corporate Enclosure Reading)").
narrative_ontology:topic_domain(copyright_constitutional_mandate__corporate_enclosure_reading, "intellectual_property_law/constitutional_law/political_economy").

domain_priors:requires_active_enforcement(copyright_constitutional_mandate__corporate_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(copyright_constitutional_mandate__corporate_enclosure_reading, '3a5a2905-6df5-4684-a611-5d987ab59889').
narrative_ontology:cs_kernel_codification('3a5a2905-6df5-4684-a611-5d987ab59889', fixed_text).
narrative_ontology:cs_authority_grounding('3a5a2905-6df5-4684-a611-5d987ab59889', extraction).
narrative_ontology:cs_interpretation_layer_present('3a5a2905-6df5-4684-a611-5d987ab59889').
narrative_ontology:cs_reading_relation('3a5a2905-6df5-4684-a611-5d987ab59889', copyright_constitutional_mandate__public_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('3a5a2905-6df5-4684-a611-5d987ab59889', copyright_constitutional_mandate__judicial_ambiguity_reading, influences).
narrative_ontology:cs_axiom('3a5a2905-6df5-4684-a611-5d987ab59889', foundational, copyright_is_natural_property_right).
narrative_ontology:cs_axiom_status(copyright_is_natural_property_right, holdable).
narrative_ontology:cs_axiom_grounding('3a5a2905-6df5-4684-a611-5d987ab59889', copyright_is_natural_property_right, deontological).
narrative_ontology:cs_axiom('3a5a2905-6df5-4684-a611-5d987ab59889', foundational, limited_times_permits_maximal_extension).
narrative_ontology:cs_axiom_status(limited_times_permits_maximal_extension, holdable).
narrative_ontology:cs_axiom_grounding('3a5a2905-6df5-4684-a611-5d987ab59889', limited_times_permits_maximal_extension, conventional).
narrative_ontology:cs_reference_frame('3a5a2905-6df5-4684-a611-5d987ab59889', founders_limited_monopoly_bargain).
narrative_ontology:cs_drift_state('3a5a2905-6df5-4684-a611-5d987ab59889', post_sonny_bono_extension_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3a5a2905-6df5-4684-a611-5d987ab59889', '').
narrative_ontology:cs_kernel_id(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_constitutional_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, major_record_labels).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, film_studio_licensing_arms).
narrative_ontology:constraint_beneficiary(copyright_constitutional_mandate__corporate_enclosure_reading, long_tail_rightsholder_estates).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, educators).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, archivists).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, documentary_filmmakers).
narrative_ontology:constraint_victim(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, copyright_as_natural_property_right).
narrative_ontology:constraint_vindicates(copyright_constitutional_mandate__corporate_enclosure_reading, authorial_labor_desert_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Own vast back catalogs approaching public-domain entry dates; lobby for term extensions (Bono Act pattern), fund litigation over fair use boundaries, and drive the DMCA anti-circumvention regime. They frame every extension as protecting 'the author's just reward' while the works at stake are typically corporate-owned works-for-hire, not living authors. They set legislative agenda through sustained, well-funded advocacy and hold licensing revenue streams that would shrink if works entered the public domain.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates, beneficiary).

% Hold master recording rights and collect licensing/sync fees; benefit from extended terms and from criminalized circumvention of DRM that prevents low-cost format-shifting or sampling without payment. Their exit option is not exit from the regime but arbitrage across jurisdictions and licensing markets they helped design.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, major_record_labels, beneficiary,
    institutional, generational, arbitrage, global).

% Monetize character and franchise IP for decades beyond any plausible incentive-to-create rationale; push term extension specifically timed to catalog expiration dates. Benefit directly from every year added to the term.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, film_studio_licensing_arms, beneficiary,
    institutional, generational, arbitrage, global).

% Heirs and trusts controlling smaller catalogs who benefit incidentally from extension but have little lobbying power of their own; ride the coattails of conglomerate advocacy without driving it.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, long_tail_rightsholder_estates, beneficiary,
    moderate, generational, constrained, national).

% Remixers, fan-fiction communities, sample-based musicians, and independent filmmakers who cannot legally build on recent-but-not-ancient works; face takedowns, licensing costs beyond their means, or criminal liability for circumventing DRM even for transformative use. Their only 'exit' is abandoning the derivative work or risking litigation they cannot afford.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, derivative_creators, payer,
    powerless, biographical, trapped, global).

% Teachers and professors who must navigate a shrunken fair-use zone and licensing costs for course materials that would otherwise be public domain under a shorter term; institutional legal caution often produces broader self-censorship than the statute strictly requires.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, educators, payer,
    moderate, biographical, constrained, national).

% Libraries and preservation institutions unable to digitize and share orphan works because rightsholders cannot be located but the works remain formally under copyright for decades; physical media decays while legal risk blocks preservation copies.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, archivists, payer,
    moderate, generational, trapped, national).

% Must clear rights or self-censor use of historically significant footage, music, and imagery that would be public domain under earlier term lengths, raising production costs and narrowing what stories can affordably be told.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, documentary_filmmakers, payer,
    moderate, biographical, constrained, global).

% Scholars and digital-humanities projects who would testify that term extension has no measurable incentive effect on already-created works, but are structurally absent from the legislative process that sets term length; their empirical findings are cited in academic literature but rarely reach the floor debate.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, public_domain_researchers, excluded,
    powerless, generational, trapped, global).

% Enacts term-extension legislation (1976 Act, 1998 Sonny Bono Act) under sustained lobbying pressure; adopts the corporate framing of copyright as a property right requiring maximal protection rather than a limited public bargain, converting a constitutional 'limited times' clause into serial extension.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, congress, agenda_setter,
    institutional, biographical, analytical, national).

% Adjudicate challenges to term extension (Eldred v. Ashcroft) and DMCA anti-circumvention provisions; under this reading, courts that uphold extensions are correctly recognizing a property right, though the same courts under a rival reading would be seen as abdicating scrutiny.
narrative_ontology:constraint_stakeholder(copyright_constitutional_mandate__corporate_enclosure_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(copyright_constitutional_mandate__corporate_enclosure_reading, legacy_entertainment_conglomerates).
narrative_ontology:fixing_cost_class(copyright_constitutional_mandate__corporate_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides creators and rightsholders a stable, predictable exclusive-rights regime that coordinates investment in creative production by guaranteeing a defined period of exclusive commercial exploitation before works pass into the commons.
% TRANSFER_FUNCTION: Moves licensing revenue, control over derivative use, and exclusion rights from the public and from downstream creators to rightsholders — increasingly corporate rather than individual — for a period extended repeatedly beyond any term that could plausibly still be incentivizing the original act of creation.
% ABSENT_VOICES: Public domain researchers and digital preservation advocates who can show empirically that term extension does not increase creative output are structurally outside the legislative drafting process, which is dominated by industry counsel and lobbyists; orphan-work advocates and remix communities are similarly unrepresented in the rooms where term length is set.
% DISAPPEARANCE_RATIONALE: If the maximal-protection reading collapsed and terms reverted toward the original 14+14 year structure (or held at length without further extension), enormous swaths of 20th-century culture would enter the public domain; licensing revenue for legacy catalogs would fall sharply, derivative and archival work would expand rapidly, and the political economy of entertainment-industry copyright lobbying would lose its central organizing project.
% FOUNDING_PROBLEM: The constitutional copyright clause was built to solve an incentive problem: without some exclusive period, creators and publishers might under-invest in producing and disseminating works, because free-riders could copy immediately. A limited monopoly was the proposed cure.
% FOUNDING_PROBLEM_CORROBORATION: Economists studying copyright term extension (e.g. amicus economists in Eldred v. Ashcroft, including several Nobel laureates) attest that extending term length for already-created works has no forward incentive effect — the problem the clause was built to solve does not exist for the works actually being extended. This corroboration comes from outside the beneficiary set; the beneficiary conglomerates themselves maintain the founding problem is still live, citing ongoing incentive needs for future creation, which is a different empirical claim than the one that justifies extending existing terms.
narrative_ontology:disappearance_verdict(copyright_constitutional_mandate__corporate_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(copyright_constitutional_mandate__corporate_enclosure_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(copyright_constitutional_mandate__corporate_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(copyright_constitutional_mandate__corporate_enclosure_reading, 0.81, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises steadily across the interval (0.45 to 0.81) tracking the two major extension events (1976, 1998) plus accumulating DMCA enforcement infrastructure. Suppression tracks closely (0.40 to 0.72) because the corporate-enclosure reading requires active enforcement — anti-circumvention criminal liability, aggressive takedown regimes, and litigation against fair-use claimants — to hold; alternatives (shorter terms, broader fair use) are not merely disfavored but actively suppressed through statutory and civil-criminal mechanisms. Theater ratio is moderate and rising (0.20 to 0.42): a real coordination function (incentivizing original creation) persists at the margin, but an increasing share of enforcement activity defends catalog value on already-created works where no forward incentive effect can plausibly operate — that is the theatrical residue, dressed as principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Legacy conglomerates and major rightsholders sit near the full-beneficiary end: they set the legislative agenda, collect licensing revenue, and have arbitrage-grade exit (they operate across jurisdictions and licensing structures they helped design). Derivative creators, educators, and archivists sit near the full-target end: trapped or constrained exit, no comparable lobbying capacity, and the extraction lands directly on their ability to build on, teach with, or preserve culture. Long-tail rightsholder estates are beneficiaries in principle but hold little independent power — they ride the conglomerates' advocacy without driving it, which is why their power atom is moderate rather than institutional despite sharing the beneficiary role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem/disappearance_verdict pair surfaces exactly the mismatch this classification exists to detect: founding_problem_status is authored as dead (economists outside the beneficiary set attest extension of existing terms has no incentive effect) while disappearance_verdict is world_rearranges (real arrangements — licensing revenue, corporate catalog value — depend on the current term length). That combination is the zombie-mandate signature: a constraint whose stated justification no longer holds but whose material stakes are enormous, which is precisely why tangled_rope (not snare) is the correct claim — a genuine coordination function existed at founding and persists at the margin for new works, but the term-extension apparatus riding on top of it is asymmetric extraction requiring active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_right_vs_limited_grant_framing,
    'Is copyright structurally a property right that the state recognizes and protects, or a limited statutory grant instrumentally created to serve the public interest in a way that happens to resemble property?',
    'Doctrinal and historical analysis of the IP Clause''s drafting history (Madison, Jefferson correspondence on limited monopolies) versus subsequent property-rights jurisprudence; comparison with the constitutional structure of actual property rights (real property has no ''limited times'' clause).',
    'If copyright is structurally a property right, the corporate-enclosure reading''s core premise holds and term extension is simply protecting property, consistent with its lower-suppression framing. If copyright is structurally a limited instrumental grant, the enclosure reading''s premise is false on its own terms and the arrangement is better read as extraction dressed in property-rights language — this is the central interpretive fork the kernel''s three readings divide on.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_right_vs_limited_grant_framing, conceptual, 'Whether copyright''s constitutional nature is property-like or grant-like — the foundational fork between this reading and its siblings.').

omega_variable(
    limited_times_maximal_extension_coherence,
    'Can ''limited times'' coherently mean ''maximal extension short of explicit perpetuity,'' or does serial extension timed to catalog expiration dates functionally create de facto perpetuity in violation of the clause''s plain text?',
    'Track whether Congress extends terms again as current-length works approach public domain entry (the pattern following 1976 and 1998); a further extension timed to Mickey Mouse''s 2024 entry into the public domain, or absence of one, is direct evidence.',
    'Repeated pattern-matched extensions would support the judicial_ambiguity_reading''s critics and the public_scaffold_reading''s claim that the enclosure reading produces functional perpetuity — undermining the enclosure reading''s claim to honor ''limited times'' at all.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(limited_times_maximal_extension_coherence, empirical, 'Whether the enclosure reading''s own account of limited-but-maximal terms is falsified by the extension pattern.').

omega_variable(
    incentive_effect_on_existing_works,
    'Does extending copyright term for already-created works produce any forward incentive effect, or does it purely transfer rent from the public/derivative creators to existing rightsholders with zero counterfactual creative benefit?',
    'Economic analysis of creative output before and after term-extension events, controlling for other factors; the Eldred v. Ashcroft amicus economists'' brief is existing evidence pointing toward zero incentive effect for backward-looking extension.',
    'If zero incentive effect is confirmed, the coordination-function claim underlying even the tangled_rope classification weakens for the extension increment specifically (though not necessarily for the base grant), pushing the extension component of this reading closer to snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_effect_on_existing_works, empirical, 'Whether retroactive term extension has any incentive effect distinguishable from pure rent transfer.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_constitutional_mandate__corporate_enclosure_reading, 1976, 2028).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copy_tr_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1976, 0.2).
narrative_ontology:measurement(copy_tr_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(copy_tr_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(copy_tr_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2008, 0.38).
narrative_ontology:measurement(copy_tr_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2018, 0.4).
narrative_ontology:measurement(copy_tr_t2028, copyright_constitutional_mandate__corporate_enclosure_reading, theater_ratio, 2028, 0.42).

% Extraction over time
narrative_ontology:measurement(copy_be_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1976, 0.45).
narrative_ontology:measurement(copy_be_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(copy_be_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 1998, 0.68).
narrative_ontology:measurement(copy_be_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2008, 0.74).
narrative_ontology:measurement(copy_be_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2018, 0.78).
narrative_ontology:measurement(copy_be_t2028, copyright_constitutional_mandate__corporate_enclosure_reading, base_extractiveness, 2028, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(copy_su_t1976, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1976, 0.4).
narrative_ontology:measurement(copy_su_t1990, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1990, 0.48).
narrative_ontology:measurement(copy_su_t1998, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 1998, 0.62).
narrative_ontology:measurement(copy_su_t2008, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2008, 0.67).
narrative_ontology:measurement(copy_su_t2018, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(copy_su_t2028, copyright_constitutional_mandate__corporate_enclosure_reading, suppression_requirement, 2028, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_constitutional_mandate__corporate_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(copyright_constitutional_mandate__corporate_enclosure_reading, 0.1).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, public_scaffold_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, judicial_ambiguity_reading).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, dmca_anticircumvention_criminalization).
narrative_ontology:affects_constraint(copyright_constitutional_mandate__corporate_enclosure_reading, fair_use_doctrine_scope).

% DUAL FORMULATION NOTE:
% This story is one of three constraints reading the same copyright_constitutional_mandate kernel. corporate_enclosure_reading (this file) authors high ε (0.81) with corporate incumbents as beneficiaries and derivative creators/educators/archivists as victims, claimed as tangled_rope. judicial_ambiguity_reading authors a structurally distinct claim (term length as legislative discretion under rational-basis review) with its own ε and stakeholder set. public_scaffold_reading authors copyright as instrumentally bounded by public-domain enrichment, with correspondingly lower ε and a different beneficiary/victim structure (or none, if genuinely bounded). The three do not average into one 'copyright' constraint — per the ε-invariance principle, each is a separate file with its own classification, linked here via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
