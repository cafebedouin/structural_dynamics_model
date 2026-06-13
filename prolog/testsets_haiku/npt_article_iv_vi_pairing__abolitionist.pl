% ============================================================================
% CONSTRAINT STORY: npt_article_iv_vi_pairing__abolitionist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_article_iv_vi_pairing__abolitionist, []).

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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV Legitimacy Constraint (Abolitionist Reading)
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   The abolitionist reading of the NPT's Article IV-VI pairing treats the
 *   treaty as a prohibition framework with unfinished business: Article VI
 *   mandates complete nuclear disarmament as a binding obligation, not an
 *   aspirational goal. Under this reading, Article IV's grant of peaceful
 *   nuclear technology access is contingent on Article VI performance; if
 *   weapon states maintain or modernize arsenals, they breach the treaty's
 *   core bargain and delegitimize both their own nuclear programs and the
 *   legal framework enabling dual-use technology export. Authority for this
 *   reading derives from humanitarian law precedent (weapon prohibition
 *   treaties: biological, chemical) and from TPNW norms that classify nuclear
 *   weapons as categorically illegal under evolving international law. This
 *   reading competes with two siblings: the nonproliferation reading (Article
 *   VI is aspirational, verification of Article III is the real constraint)
 *   and the grand_bargain reading (Articles IV and VI are reciprocal, breach
 *   of VI undermines but does not delegitimize IV). The abolitionist reading
 *   is a kernel reading — one interpretation of a contested NPT kernel that
 *   stabilizes legitimacy through weapon prohibition rather than through
 *   state-security-based nonproliferation. It is neither the dominant
 *   canonical reading nor a fringe position; it holds significant
 *   institutional support from non-aligned movement states, humanitarian law
 *   scholars, and TPNW coalition actors, and faces systematic suppression
 *   from weapon states and their security-dependent allies.
 *
 * KEY AGENTS:
 *   - Non-nuclear weapon states (organized): benefit from the reading's inversion of obligation — they gain standing to demand weapon-state compliance and to condition their own restraint on disarmament progress.
 *   - Nuclear weapon states (institutional): bear the cost of being named obligated parties to binding disarmament; maintaining arsenals becomes treaty breach under this reading.
 *   - Dual-use technology exporters (powerful): lose the presumption that peaceful intent justifies export; face stricter scrutiny and potential liability under the reading's proliferation-risk constraint.
 *   - Humanitarian law advocates & TPNW coalition (organized): benefit by having a reading that aligns NPT interpretation with prohibition norms and vindicates their position that weapons are categorically illegal.
 *   - Non-aligned movement states (organized): pay by being caught between the reading's moral force and institutional cost — endorsing it risks security consequences; endorsing competing readings appears to condone proliferation.
 *   - International Court of Justice (observer): holds interpretive authority that could institutionalize or marginalize this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.72).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV Legitimacy Constraint (Abolitionist Reading)").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'aeddee1e-fb53-42c6-8380-fb8b1190f2ab').
narrative_ontology:cs_kernel_codification('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', fixed_text).
narrative_ontology:cs_authority_grounding('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', lineage).
narrative_ontology:cs_interpretation_layer_present('aeddee1e-fb53-42c6-8380-fb8b1190f2ab').
narrative_ontology:cs_reading_relation('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', foundational, nuclear_weapons_inherently_prohibited).
narrative_ontology:cs_axiom_status(nuclear_weapons_inherently_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', nuclear_weapons_inherently_prohibited, deontological).
narrative_ontology:cs_axiom('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', foundational, article_vi_binding_disarmament).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament, holdable).
narrative_ontology:cs_axiom_grounding('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', article_vi_binding_disarmament, deontological).
narrative_ontology:cs_axiom('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', secondary, prohibition_treaty_precedent).
narrative_ontology:cs_axiom_status(prohibition_treaty_precedent, holdable).
narrative_ontology:cs_axiom_grounding('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', prohibition_treaty_precedent, empirically_contingent).
narrative_ontology:cs_reference_frame('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', weapons_prohibition_genealogy).
narrative_ontology:cs_drift_state('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', contemporary_humanitarian_law_ascendance, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('aeddee1e-fb53-42c6-8380-fb8b1190f2ab', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, humanitarian_norm_advocates).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_exporters).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_article_iv_vi_pairing__abolitionist_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_article_iv_vi_pairing__abolitionist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the reading imposes new obligations on weapon states (binding disarmament) and restrictions on exporters (dual-use technology scrutiny) without offering them compensatory legitimacy or security benefits — they experience it as extraction of obligation without reciprocal gain. The constraint persists not because weapon states accept it but because enough non-weapon states and advocate organizations maintain the reading's institutional presence (ICJ advisory opinions, General Assembly votes, TPNW protocol development). Suppression is substantial (0.72) because the reading's enforcement depends on sustained diplomatic pressure from non-weapon states against the structural power of weapon-state veto and security guarantees; the suppression is built into the institutional asymmetry, not imposed top-down. Theater ratio is moderate (0.41) because the reading's legitimacy framing (humanitarian law, weapons prohibition) is genuine, but a growing share of institutional activity is devoted to defending the weapon states' right to maintain arsenals against the reading's constraint, rather than to achieving actual disarmament. Measurement series show rising extractiveness and suppression over the interval, reflecting the reading's increasing institutional salience (more states adopting it, more ICJ scrutiny, more TPNW expansion) and the corresponding hardening of weapon-state resistance.
 *
 * PERSPECTIVAL GAP:
 *   The weapon-state seat and the non-weapon-state seat should compute entirely different types from the same structural data. From the weapon-state position, this reading is illegitimate constraint (extraction without consent), justified only by humanitarian framing that de-legitimizes their security model — they experience it as snare-like (coercive obligation imposed despite veto rights, suppressed by institutional coalition pressure). From the non-weapon-state position, the reading is genuine rope (coordination of disarmament obligation that undoes the asymmetry of the founding bargain); from the exporter position, it is snare-like (restriction of legitimate business activity). The engine computes these divergences from stakeholder power/exit + beneficiary/victim roles. The authored claim (tangled_rope) reflects the reading's own framing: it solves a real coordination problem (making disarmament binding achieves mutual security better than aspirational goals) while creating asymmetric extraction (weapon states pay, non-weapon states gain leverage). The claim/metric gap is deliberate: the metrics describe how the constraint actually operates (substantially extractive, requiring sustained suppression), independent of whether that operation is justified.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-weapon states benefit structurally: the reading grants them standing to contest weapon-state behavior and to condition their own obligations on disarmament progress. Their d value is near the beneficiary end (0.15-0.25) because they gain leverage without bearing extraction costs directly. Nuclear weapon states are the identified targets: they bear new binding obligations (disarmament) with suppressive enforcement (coalition pressure, ICJ scrutiny) and no compensatory benefit. Their d value is near the target end (0.75-0.85) because the reading constrains their behavior and they experience it as extraction. Dual-use exporters are secondary targets: technology restrictions reduce their business options. Humanitarian advocates are beneficiaries (the reading vindicates their normative position). Non-aligned movement states occupy an intermediate position (pay by being caught between readings, benefit by gaining rhetorical leverage) — d around 0.50-0.60. The directionality_overrides are not needed because the structural data (beneficiary declarations, victim declarations, power/exit combinations) already derive the right d values.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mandatrophy through active contestation. The founding problem (horizontal proliferation risk) remains live, and the abolitionist reading addresses it by making disarmament binding. However, there is a secondary mandatrophy risk: if weapon states continue to modernize arsenals unopposed, the reading's mandate (disarmament) becomes dead while its enforcement machinery persists (verification regimes, export controls continue). The measurement series models this risk through rising extractiveness: as the gap between the reading's mandate (disarmament) and actual weapon-state behavior (modernization) widens, the constraint's extractive character becomes more visible and its legitimacy erodes unless enforcement mechanisms adapt. The reading is currently in the live-mandate phase because non-weapon states actively invoke it and ICJ processes are engaged; it would enter mandatrophy if those voices fell silent but the verification and export machinery continued as routine bureaucracy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_justiciability,
    'Is Article VI justiciable? Does it create binding legal obligations with specific performance metrics, or is it an aspirational statement subject to state discretion?',
    'International Court of Justice advisory opinion or compulsory jurisdiction case on Article VI compliance; examination of treaty travaux préparatoires and subsequent state practice.',
    'If justiciable and binding, the abolitionist reading becomes institutionally defensible and can support enforcement mechanisms (sanctions, treaty withdrawal, compulsory arbitration). If aspirational, the nonproliferation reading''s framing (security-based discretion is legitimate) gains canonical status and the abolitionist reading becomes advocacy rather than legal interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_justiciability, conceptual, 'Whether Article VI imposes binding legal obligations or advisory commitments.').

omega_variable(
    humanitarian_law_supremacy_over_security,
    'When humanitarian law norms (weapons prohibition) conflict with state security interests (deterrence, extended guarantees), which takes precedence in treaty interpretation?',
    'Jurisprudence on the hierarchy of international law norms; examination of how other weapons prohibition treaties were adjudicated when security arguments were raised; ICJ interpretation of ius cogens (peremptory norms) applicability to nuclear weapons.',
    'If humanitarian law supremacy is established, the abolitionist reading becomes canonical; weapon states'' security justifications for arsenal maintenance are legally foreclosed. If security interests retain coequal standing, the grand_bargain reading (security-based reciprocal obligation) becomes canonical and the abolitionist reading is marginalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_law_supremacy_over_security, conceptual, 'Whether humanitarian norms or state security interests govern treaty interpretation hierarchy.').

omega_variable(
    dual_use_technology_non_diversion_assumption,
    'Under what conditions does peaceful-intent justification for dual-use technology export remain valid? If proliferation pathways are possible, is export prohibited regardless of stated intent?',
    'Regulatory changes to export control regimes; case law on technology transfer liability; technical assessments of whether dual-use pathways can be adequately monitored and prevented.',
    'If the abolitionist reading prevails, the non-diversion assumption collapses: any technology enabling weapons development is prohibited regardless of peaceful framing. If the nonproliferation reading prevails, the assumption holds: export is lawful if intent is peaceful and verification mechanisms exist. This directly affects dual-use exporters'' business model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dual_use_technology_non_diversion_assumption, empirical, 'Whether peaceful-intent justification remains valid for dual-use technology export under proliferation risk.').

omega_variable(
    reading_institutional_consolidation,
    'Will the abolitionist reading achieve institutional consolidation (ICJ canonical status, major power adoption, treaty amendment), or will it remain a minority advocacy position lacking enforcement capacity?',
    'Tracking ICJ advisory opinions and compulsory cases; monitoring state practice and voting patterns in the UN General Assembly; observing TPNW protocol development and NPT review conference outcomes over the next 15-20 years.',
    'Consolidation would shift the NPT toward binding disarmament and de-legitimize arsenal maintenance; non-consolidation would leave the reading as a powerful moral claim without institutional teeth, and the nonproliferation reading would remain canonical. This is a persistence question, not a truth question — the reading''s structural claim is stable regardless of institutional success.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_institutional_consolidation, empirical, 'Whether institutional momentum favors abolitionist over competing readings.').

omega_variable(
    kernel_reading_contestation_nature,
    'Is the contest between readings a substantive disagreement about what the NPT text requires, or a veiled disagreement about what international law should require regardless of the treaty?',
    'Textual analysis of Article VI wording and negotiating history; examination of whether advocates of each reading cite the same textual evidence or different standards of interpretation (originalist vs. evolutionary); assessment of whether non-signatory advocacy (TPNW coalition positions) is imported into NPT interpretation or kept distinct.',
    'If substantive disagreement, the readings are genuinely competitive interpretations of the kernel. If veiled, the abolitionist reading is a normative project presented as legal interpretation, which affects its legitimacy status (advocacy is valid but not judicial interpretation). Affects how institutional actors (ICJ, treaty bodies) should frame their reasoning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_nature, conceptual, 'Whether reading contest is about textual interpretation or about what law should be.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.22).
narrative_ontology:measurement(npt__tr_t7, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 7, 0.26).
narrative_ontology:measurement(npt__tr_t14, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 14, 0.31).
narrative_ontology:measurement(npt__tr_t21, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 21, 0.36).
narrative_ontology:measurement(npt__tr_t35, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 35, 0.4).
narrative_ontology:measurement(npt__tr_t50, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(npt__be_t7, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(npt__be_t14, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 14, 0.66).
narrative_ontology:measurement(npt__be_t21, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 21, 0.71).
narrative_ontology:measurement(npt__be_t35, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 35, 0.75).
narrative_ontology:measurement(npt__be_t50, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(npt__su_t7, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(npt__su_t14, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 14, 0.66).
narrative_ontology:measurement(npt__su_t21, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 21, 0.69).
narrative_ontology:measurement(npt__su_t35, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 35, 0.71).
narrative_ontology:measurement(npt__su_t50, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.18).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_nuclear_weapon_prohibition).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_export_licensing).

% DUAL FORMULATION NOTE:
% The NPT's Article IV-VI pairing is a contested kernel with three structurally distinct readings: abolitionist (this constraint), nonproliferation_primary, and grand_bargain. Each reading assigns different ε values and different beneficiary/victim structures because they interpret the same treaty text as authorizing different obligations. The three constraints form a family linked by network.affects_constraints. The abolitionist reading influences downstream dual-use export constraints by making proliferation risk the paramount criterion; it forecloses the nonproliferation reading's claim that security interests can justify arsenal maintenance; it coexists with the grand_bargain reading (both demand disarmament, but differ on the condition and justiciability). Decomposition is necessary because ε differs significantly: nonproliferation reading has low extraction (weapon states accept verification in exchange for non-weapon-state restraint, balanced deal); abolitionist reading has high extraction (weapon states bear binding disarmament obligation as a constraint on their sovereignty); grand_bargain has medium extraction (reciprocal obligation with contingency structures).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
