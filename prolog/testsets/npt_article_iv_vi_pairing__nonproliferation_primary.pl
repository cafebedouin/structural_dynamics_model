% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__nonproliferation_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__nonproliferation_primary, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__nonproliferation_primary
 *   human_readable: NPT Article IV/VI Pairing: Non-Proliferation Primary Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The Non-Proliferation Treaty (1968) established a two-tier nuclear order:
 *   five weapon states (P5) retain legal arsenals and permanent Security
 *   Council veto; over 180 non-weapon states forgo weapons and submit to
 *   intrusive IAEA inspections under Article III. Article IV promised
 *   non-weapon states unrestricted access to civilian nuclear technology for
 *   peaceful development, conditional on verification. Article VI committed
 *   weapon states to pursue 'good faith' negotiations toward complete
 *   disarmament. The nonproliferation_primary reading holds that this
 *   arrangement is structurally permanent: Article IV benefits are available
 *   only to states that accept Article III verification; Article VI
 *   disarmament is aspirational and non-binding; the authority to enforce the
 *   system derives from the weapon states' structural security interest in
 *   preventing horizontal proliferation and retaining strategic advantage.
 *   This reading treats the two-tier order as a stable equilibrium, not a
 *   temporary bargain awaiting disarmament. The constraint exhibits rising
 *   extractiveness over the interval (1970–2024), theater ratio climbing from
 *   0.15 to 0.51, and suppression rising from 0.48 to 0.72, indicating that
 *   as Article VI enforcement atrophied, the performance of disarmament
 *   commitment increased while the actual functional purpose shifted wholly
 *   to horizontal proliferation control.
 *
 * KEY AGENTS:
 *   - weapon_states (P5): Agenda-setters; retain arsenals and veto; set verification standards for non-weapon states; exempt their own arsenals from disarmament timelines
 *   - non_weapon_states (180+): Payers and secondary beneficiaries; submit to intrusive IAEA inspections; forgo weapons development; receive conditional civilian nuclear assistance
 *   - nuclear_developing_nations: Victims; face tightest Article III constraints; denied enrichment/reprocessing technology; identity-locked into NPT compliance
 *   - international_security_apparatus (IAEA, UN Security Council): Agenda-setters and beneficiaries; administers verification and enforcement; derives institutional authority and budget from horizontal proliferation threat
 *   - abolitionist_delegations (100+ non-weapon states, humanitarian coalitions): Excluded; argue Article VI is mandatory; voted for TPNW; overridden by consensus rules
 *   - treaty_scholars_nonproliferation_school: Observers; document the interpretive evolution from 'final disarmament' (1960s) to 'good faith efforts' (1980s) to purely aspirational (2010s)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, 0.68).
domain_priors:suppression_score(npt_article_iv_vi_pairing__nonproliferation_primary, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__nonproliferation_primary, 0.51).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 0.51).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, accessibility_collapse, 0.74).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__nonproliferation_primary, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__nonproliferation_primary, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__nonproliferation_primary, "NPT Article IV/VI Pairing: Non-Proliferation Primary Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__nonproliferation_primary, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__nonproliferation_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__nonproliferation_primary, 'ac2400e8-854e-4b6c-a404-90f30be50fce').
narrative_ontology:cs_kernel_codification('ac2400e8-854e-4b6c-a404-90f30be50fce', fixed_text).
narrative_ontology:cs_authority_grounding('ac2400e8-854e-4b6c-a404-90f30be50fce', extraction).
narrative_ontology:cs_interpretation_layer_present('ac2400e8-854e-4b6c-a404-90f30be50fce').
narrative_ontology:cs_reading_relation('ac2400e8-854e-4b6c-a404-90f30be50fce', npt_article_iv_vi_pairing__grand_bargain, forecloses).
narrative_ontology:cs_reading_relation('ac2400e8-854e-4b6c-a404-90f30be50fce', npt_article_iv_vi_pairing__abolitionist, forecloses).
narrative_ontology:cs_axiom('ac2400e8-854e-4b6c-a404-90f30be50fce', foundational, two_tier_order_permanent).
narrative_ontology:cs_axiom_status(two_tier_order_permanent, holdable).
narrative_ontology:cs_axiom_grounding('ac2400e8-854e-4b6c-a404-90f30be50fce', two_tier_order_permanent, empirically_contingent).
narrative_ontology:cs_axiom('ac2400e8-854e-4b6c-a404-90f30be50fce', foundational, article_vi_aspirational_non_justiciable).
narrative_ontology:cs_axiom_status(article_vi_aspirational_non_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('ac2400e8-854e-4b6c-a404-90f30be50fce', article_vi_aspirational_non_justiciable, conventional).
narrative_ontology:cs_reference_frame('ac2400e8-854e-4b6c-a404-90f30be50fce', structural_realism_security_hierarchy).
narrative_ontology:cs_drift_state('ac2400e8-854e-4b6c-a404-90f30be50fce', contemporary_post_2015, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ac2400e8-854e-4b6c-a404-90f30be50fce', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__nonproliferation_primary, international_security_apparatus).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_developing_nations).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__nonproliferation_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__nonproliferation_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__nonproliferation_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__nonproliferation_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at the treaty's inception (when the bargain language emphasized reciprocity) to 0.68 in 2024 (when Article VI enforcement has effectively ceased). Theater_ratio climbs from 0.15 to 0.51, indicating that performance of disarmament commitment (reviews, negotiations, committees) increasingly outweighs actual functional disarmament activity—the constraint's coordination function (preventing horizontal proliferation) persists, but the equity function (reciprocal weapon-state restraint) has atrophied into theater. Suppression requirement rises from 0.48 to 0.72, tracking the growing institutional effort needed to keep non-weapon states committed despite the unilateral breach of Article VI. The coercion grid shows increasing stratification: at the structural level (great-power security architecture), accessibility collapse rises from 0.62 to 0.74 because the two-tier order is now treated as permanent fact; at the class level (non-aligned movements, developing states seeking nuclear energy), collapse rises from 0.48 to 0.68, indicating those groups have discovered they cannot exit; at the individual level (scientists, engineers, diplomats seeking fuel-cycle technology), collapse rises from 0.35 to 0.58, indicating constrained but not total alternatives. Resistance also stratifies inversely: structural resistance declines from 0.72 to 0.48 (the P5 maintain dominance), while individual resistance rises from 0.55 to 0.62 (scientists and policymakers in non-weapon states increasingly challenge the reading). This pattern is diagnostic of mandatrophy: a constraint born as reciprocal bargain whose mandate (disarmament) has died, but whose extraction mechanism (verification/restraint) persists and has hardened because no actor benefits enough from fixing it to do so—the weapon states benefit from the status quo, non-weapon states are locked in, and the IAEA's institutional interest aligns with indefinite verification.
 *
 * PERSPECTIVAL GAP:
 *   The weapon_states agenda-setter seat should compute as experiencing low extractiveness (they benefit from the arrangement without bearing verification costs, and retain strategic advantage). The non_weapon_states payer seats should compute as experiencing high extractiveness and suppression (they bear asymmetric verification and restraint costs, with deferred or absent reciprocation). The nuclear_developing_nations victim seat should compute as identity_locked, constrained in exit, and bearing maximal suppression because they need nuclear technology for development but are most tightly restricted and face reputational cost from NPT withdrawal. The international_security_apparatus beneficiary seats (IAEA, UN SC) should compute as deriving institutional authority from the continued salience of the proliferation threat, thus having structural interest in maintaining suppression. The abolitionist_delegations excluded seat should compute as powerless to shift the interpretation despite organizational strength, because consensus-gate rules require weapon-state agreement. The engine's per-seat classification should show divergence: weapon-state perspective = stable coordination (preventing horizontal proliferation solves a real problem); non-weapon-state perspective = extraction (asymmetric restraint without reciprocal disarmament); the analytical observer seat should recognize the constraint as tangled_rope (genuine coordination function + asymmetric extraction) with rising theatrical maintenance, indicating mandatrophy.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states derive d near 0.0 (full beneficiary): they set the rules, collect strategic advantage from the two-tier order, and face no verification. Non-weapon states derive d near 1.0 (full target): they bear verification costs, restraint on development, and deferred reciprocation. Nuclear_developing_nations derive even higher d (identity-locked exit raises the effective cost of disobedience). The directionality for abolitionist delegations sits at d~0.85 (high target): they are systematically overridden despite their power category (organized), because structural veto rules prevent their interpretation from changing the constraint. Nuclear_suppliers_cartel derives moderate d (~0.35) despite being in beneficiary role: they benefit from restricted technology access, but are not the primary extraction-collector—they are more like secondary beneficiaries riding on the weapon states' enforcement of the regime. No directionality overrides are needed; the derivation from beneficiary/victim data, exit options, and power atoms should produce the correct stratification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits textbook mandatrophy (OQ-80): (1) founding mandate = reciprocal disarmament (Article VI); (2) mandate status = effectively dead (no disarmament timelines enforced, no breach consequences); (3) constraint persistence = intact (non-weapon states still bound by Article III, IAEA inspections still conducted); (4) beneficiary shift = from mutual security (1970) to asymmetric advantage (2024); (5) theater_ratio rise = from 0.15 (mostly functional verification) to 0.51 (substantial share of activity is review conferences, negotiating committees, and declarations of commitment with zero enforcement). The constraint persists not because it solves its founding problem (disarmament) but because it solves a derivative problem (horizontal proliferation) that serves weapon states' structural security interest. Non-weapon states cannot exit because NPT withdrawal triggers security isolation and loss of nuclear technology access (identity-locked). Weapon states cannot fix the constraint because they benefit from its asymmetric operation. The IAEA administrator cannot unilaterally change it because its authority derives from the P5-controlled UN system. The result is a zombie constraint: the founding mandate is abandoned, the extraction mechanism persists and hardens, and theater (review conferences, 'good faith' declarations, committees) substitutes for actual function. A genuine fix would require either (a) enforcement of Article VI against weapon states (currently impossible due to their veto), (b) non-weapon-state coalition exit from the NPT (currently prevented by identity-lock and reputational cost), or (c) replacement by a new framework like the TPNW (currently excluded by weapon-state non-accession and institutional hierarchy). Mandatrophy_resolved is false; the constraint's mandate (disarmament) remains theoretically active in the treaty text but practically abandoned, and no seat has sufficient power and incentive to resolve the contradiction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_dormancy_vs_death,
    'Is Article VI merely dormant (still binding, awaiting enforcement activation) or substantively dead (abandoned by consensus among weapon states and accepted by non-weapon states as unenforceable)?',
    'A weapon state taking unilateral action to enforce Article VI disarmament (e.g., submitting to international court, accepting binding verification of arsenal reduction) would demonstrate revival potential; continued unanimous inaction by all P5 for another 20+ years would demonstrate substantive death.',
    'If dormant, the constraint could be reclassified from mandatrophy to deferred_reciprocal; if dead, mandatrophy is confirmed and the founding bargain is structurally broken. This resolution affects whether non-weapon states have legitimate grounds to withdraw.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_dormancy_vs_death, empirical, 'Whether Article VI remains a live obligation or has been effectively abandoned.').

omega_variable(
    two_tier_permanence_vs_transitional,
    'Is the two-tier order permanently stabilized (as nonproliferation_primary reading holds) or transitional (as grand_bargain reading suggests), pending actual weapon-state disarmament?',
    'The grand_bargain reading predicts that significant weapon-state arsenal reductions (e.g., 80% cut in deployed warheads, binding international verification) would restore Article IV legitimacy and re-couple the tiers. The nonproliferation_primary reading predicts that such reductions would not occur because they are not in weapon states'' security interest. If comprehensive disarmament occurs, grand_bargain interpretation is vindicated; if arsenals remain stable or grow over the next 30 years despite non-weapon-state pressure, nonproliferation_primary''s permanence claim is vindicated.',
    'If transitional, the constraint is a temporary stage of a larger bargain and could reclassify toward scaffold (with deferred sunset). If permanent, the constraint is a stable two-tier architecture and remains tangled_rope with mandatrophy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(two_tier_permanence_vs_transitional, conceptual, 'Whether the NPT''s two-tier order is permanent or transitional to a disarmed world.').

omega_variable(
    horizontal_proliferation_threat_reality,
    'Is the horizontal proliferation threat (the justification for asymmetric Article III verification) a genuine security imperative or a constructed narrative used to legitimize restraint of non-weapon states?',
    'Empirical analysis: (a) compare the actual rate of proliferation attempts (Iran, North Korea, Libya, etc.) to the baseline rate predicted if the NPT did not exist; (b) measure the IAEA''s detection rate for diversion and near-diversion; (c) assess whether non-weapon states that achieved high technical capacity (Japan, South Korea, Germany) were deterred by the NPT or only by strategic choice. If attempts are rarer and deterrence strong, the threat is real; if the same states could have pursued weapons without the NPT''s verification (but chose not to for security/economic reasons), the threat is partially constructed.',
    'If the threat is entirely constructed, the constraint''s coordination function collapses and it becomes pure extraction. If the threat is real but overstated, extraction is partially justified as coordination cost. The reading''s authority claim (deriving from weapon states'' security interest in preventing horizontal proliferation) depends on the threat''s reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(horizontal_proliferation_threat_reality, empirical, 'Whether horizontal proliferation is a genuine security threat or a legitimizing narrative.').

omega_variable(
    reading_kernel_foreclosure,
    'Does the nonproliferation_primary reading''s core premise (that the two-tier order is permanent and legitimate) logically foreclose the grand_bargain reading (that the tiers are reciprocally bound and Article VI breach undermines Article IV)?',
    'Logical analysis: if nonproliferation_primary holds that Article VI is purely aspirational and non-binding, then the grand_bargain''s claim that Article VI breach undermines Article IV legitimacy is false by construction. A party cannot hold both readings simultaneously in the same framework. However, different parties can hold the readings (weapon states prefer nonproliferation_primary; non-aligned states prefer grand_bargain). The question is whether the readings are logically incompatible or merely practically opposed.',
    'If foreclosure: the relation nonproliferation_primary→grand_bargain should be ''forecloses''. If coexistence: the relation should be ''coexists_with''. This affects how the engine models the contested kernel''s structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_foreclosure, conceptual, 'Whether nonproliferation_primary and grand_bargain readings logically foreclose each other or coexist as party-dependent interpretations.').

omega_variable(
    suppression_mechanism_internalization,
    'Is the rising suppression of non-weapon states'' resistance to the NPT''s asymmetry structural (external barriers: technology denial, reputational cost, isolation) or internalized (non-weapon states have accepted the two-tier order as legitimate and natural)?',
    'Post-exit trajectory: if a non-weapon state withdraws from the NPT and faces external suppression (sanctions, diplomatic isolation) but later reports internal acceptance of restraint (''we chose not to pursue weapons''), the suppression is partially internalized. If the same state pursues weapons aggressively after withdrawal, suppression was purely structural. The TPNW''s non-weapon-state support (120+ signatures) suggests that many non-weapon states have NOT internalized the NPT''s asymmetry—their suppression is structural, not internalized belief.',
    'If internalized, the constraint''s effective suppression is higher than the scalar metric (0.72) suggests, and exit costs include psychological/identity burden. If structural, the constraint is brittle: withdrawal removes suppression. The reading''s authority claim depends on whether constraint acceptance is volitional or coerced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether non-weapon states'' acceptance of NPT asymmetry is internalized or structurally suppressed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__nonproliferation_primary, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 1985, 0.25).
narrative_ontology:measurement(npt__tr_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2000, 0.38).
narrative_ontology:measurement(npt__tr_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2010, 0.45).
narrative_ontology:measurement(npt__tr_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2018, 0.49).
narrative_ontology:measurement(npt__tr_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, theater_ratio, 2024, 0.51).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(npt__be_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(npt__be_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(npt__be_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(npt__be_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2018, 0.66).
narrative_ontology:measurement(npt__be_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1970, 0.48).
narrative_ontology:measurement(npt__su_t1985, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 1985, 0.58).
narrative_ontology:measurement(npt__su_t2000, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2000, 0.64).
narrative_ontology:measurement(npt__su_t2010, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(npt__su_t2018, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2018, 0.71).
narrative_ontology:measurement(npt__su_t2024, npt_article_iv_vi_pairing__nonproliferation_primary, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__nonproliferation_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__nonproliferation_primary, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, npt_article_iv_vi_pairing__abolitionist).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, iaea_safeguards_authority).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__nonproliferation_primary, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% The NPT Article IV/VI constraint family decomposes into three readings of a single contested kernel (npt_article_iv_vi_pairing): nonproliferation_primary (this file, treating two-tier order as permanent), grand_bargain (treating tiers as reciprocally bound), and abolitionist (treating Article VI as mandatory disarmament and two-tier order as illegitimate). Each reading has distinct ε (effectiveness of extraction), distinct beneficiary/victim structures, and distinct authority grounding. The readings are not alternative measurements of one constraint; they are genuinely different constraints instantiated by different parties' interpretation of the same treaty text. The nonproliferation_primary reading exhibits rising mandatrophy (0.35→0.68 extractiveness, 0.15→0.51 theater_ratio) because the founding mandate (reciprocal disarmament) has been abandoned while the extraction mechanism (Article III verification/restraint) persists. The grand_bargain reading would exhibit different metrics (beneficiaries would include non-weapon states, extractiveness would be lower because reciprocal obligation is supposed to bind weapon states). The abolitionist reading would show even higher extractiveness and suppression because the entire two-tier order is characterized as illegitimate. All three readings affect IAEA authority structures and nuclear suppliers' cartel gatekeeping; the readings are linked through structural influence (nonproliferation_primary creates downstream pressure on grand_bargain and abolitionist by normalizing the two-tier order, while grand_bargain influences nonproliferation_primary by contesting its authority claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__nonproliferation_primary, organized, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
