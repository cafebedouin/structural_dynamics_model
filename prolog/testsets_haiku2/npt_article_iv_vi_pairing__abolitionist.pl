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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: npt_article_iv_vi_pairing__abolitionist
 *   human_readable: NPT Article IV-VI Pairing Under Abolitionist Reading
 *   domain: international_law/nuclear_governance
 *
 * SUMMARY:
 *   This constraint story instantiates the ABOLITIONIST reading of the
 *   contested NPT Article IV-VI pairing. Under this reading, Article VI's
 *   mandate for complete nuclear disarmament is legally binding, not
 *   aspirational. Article IV's peaceful-use access is illegitimate to the
 *   extent it perpetuates dual-use proliferation risk and weapon states'
 *   monopoly on enrichment technology. Authority derives from humanitarian
 *   law precedent (weapons prohibition norms) and the TPNW's 2017
 *   codification of categorical prohibition. The constraint operates as a
 *   tangled rope: weapon states extract security monopoly rent and
 *   technology-export rents while non-weapon states are coordinated into
 *   restraint through promises of disarmament and peaceful access that remain
 *   unfulfilled. The abolitionist reading reads the NPT as increasingly
 *   performative—ceremonies of disarmament review without institutional
 *   capacity to enforce binding timelines. The claim/metric gap is
 *   intentional: the constraint is AUTHORED as tangled_rope (real
 *   coordination problem + asymmetric extraction + active enforcement), and
 *   the metrics honestly reflect that structure. This reading diverges
 *   sharply from the nonproliferation_primary reading (which treats Article
 *   VI as non-justiciable) and influences but does not foreclose the
 *   grand_bargain reading (which holds both articles as reciprocal but
 *   enforceable).
 *
 * KEY AGENTS:
 *   - Weapon states: institutional agenda-setters, retain nuclear arsenals, control treaty interpretation through Security Council veto and review-conference procedure.
 *   - Non-weapon states: organized payers, constrained by inspection obligations and technology restrictions, receive promised disarmament that does not materialize.
 *   - Humanitarian constituencies: powerless, excluded from treaty machinery, advocate for categorical prohibition under TPNW frame.
 *   - Dual-use suppliers: beneficiaries, profit from ambiguous Article IV language that does not distinguish peaceful from military pathways.
 *   - Treaty verification (IAEA): structural observer, operates within weapon-state mandate, produces technical analysis but does not adjudicate legitimacy.
 *   - Abolitionist advocates: analytical observers, argue binding disarmament obligation and humanitarian law supremacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_article_iv_vi_pairing__abolitionist, 0.78).
domain_priors:suppression_score(npt_article_iv_vi_pairing__abolitionist, 0.71).
domain_priors:theater_ratio(npt_article_iv_vi_pairing__abolitionist, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, extractiveness, 0.78).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(npt_article_iv_vi_pairing__abolitionist, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_article_iv_vi_pairing__abolitionist, tangled_rope).
narrative_ontology:human_readable(npt_article_iv_vi_pairing__abolitionist, "NPT Article IV-VI Pairing Under Abolitionist Reading").
narrative_ontology:topic_domain(npt_article_iv_vi_pairing__abolitionist, "international_law/nuclear_governance").

domain_priors:requires_active_enforcement(npt_article_iv_vi_pairing__abolitionist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_article_iv_vi_pairing__abolitionist, 'ffb10021-987a-480c-8ac3-607d6f037f35').
narrative_ontology:cs_kernel_codification('ffb10021-987a-480c-8ac3-607d6f037f35', fixed_text).
narrative_ontology:cs_authority_grounding('ffb10021-987a-480c-8ac3-607d6f037f35', extraction).
narrative_ontology:cs_interpretation_layer_present('ffb10021-987a-480c-8ac3-607d6f037f35').
narrative_ontology:cs_reading_relation('ffb10021-987a-480c-8ac3-607d6f037f35', npt_article_iv_vi_pairing__nonproliferation_primary, forecloses).
narrative_ontology:cs_reading_relation('ffb10021-987a-480c-8ac3-607d6f037f35', npt_article_iv_vi_pairing__grand_bargain, coexists_with).
narrative_ontology:cs_axiom('ffb10021-987a-480c-8ac3-607d6f037f35', foundational, article_vi_complete_disarmament_binding).
narrative_ontology:cs_axiom_status(article_vi_complete_disarmament_binding, holdable).
narrative_ontology:cs_axiom_grounding('ffb10021-987a-480c-8ac3-607d6f037f35', article_vi_complete_disarmament_binding, deontological).
narrative_ontology:cs_axiom('ffb10021-987a-480c-8ac3-607d6f037f35', foundational, humanitarian_law_supremacy_over_dual_use).
narrative_ontology:cs_axiom_status(humanitarian_law_supremacy_over_dual_use, holdable).
narrative_ontology:cs_axiom_grounding('ffb10021-987a-480c-8ac3-607d6f037f35', humanitarian_law_supremacy_over_dual_use, deontological).
narrative_ontology:cs_axiom('ffb10021-987a-480c-8ac3-607d6f037f35', secondary, weapons_prohibition_norm_codified).
narrative_ontology:cs_axiom_status(weapons_prohibition_norm_codified, holdable).
narrative_ontology:cs_axiom_grounding('ffb10021-987a-480c-8ac3-607d6f037f35', weapons_prohibition_norm_codified, empirically_contingent).
narrative_ontology:cs_reference_frame('ffb10021-987a-480c-8ac3-607d6f037f35', humanitarian_law_weapons_abolition).
narrative_ontology:cs_drift_state('ffb10021-987a-480c-8ac3-607d6f037f35', contemporary_2026, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ffb10021-987a-480c-8ac3-607d6f037f35', '').
narrative_ontology:cs_kernel_id(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, weapon_states).
narrative_ontology:constraint_beneficiary(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_suppliers).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, non_weapon_states).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, humanitarian_constituencies).
narrative_ontology:constraint_victim(npt_article_iv_vi_pairing__abolitionist, future_generations).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, categorical_weapons_prohibition_norm).
narrative_ontology:constraint_vindicates(npt_article_iv_vi_pairing__abolitionist, humanitarian_law_supremacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Five states retain nuclear arsenals and set the interpretation of NPT through Security Council veto, permanent institutional seats, and treaty-review conference participation. They frame Article IV as protecting peaceful use rights and Article VI as aspirational disarmament language, not binding obligation. They enforce this reading through non-verification of disarmament claims and technological gatekeeping over non-weapon states' access to dual-use materials. Their exit option is withdrawal (India, Pakistan, North Korea precedent); their staying power derives from nuclear deterrence doctrine.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, weapon_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Accept restraint on weapons development and submit to verification inspections in exchange for promised access to peaceful nuclear technology and a disarmament process that has not materialized in 54 years. Under the abolitionist reading, they bear the cost of foreclosed weapons sovereignty while weapon states collect the security rent of permanent nuclear monopoly. Their exit is costly (withdrawal triggers isolation) and their alternatives are captured (enrichment technologies remain restricted).
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, non_weapon_states, payer,
    organized, biographical, constrained, global).

% Advocate for nuclear abolition and cite humanitarian catastrophe risk, but are structurally excluded from NPT interpretation machinery (no permanent seats, no veto, no treaty-review negotiating power). The TPNW they signed is treated as illegitimate by weapon states and their allies. They would argue the NPT's Article VI language is legally binding and Article IV cannot override prohibition norms, but their voice is absent from the institutional design.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, humanitarian_constituencies, excluded,
    powerless, biographical, trapped, global).

% Export enrichment and reprocessing technology, reactor designs, and fissile material under the cover of Article IV peaceful-use rights. They benefit from the institutional ambiguity: the constraint's lack of enforced distinction between peaceful and military pathways leaves profitable markets open. They have exit options (serve non-NPT states, offer commercial non-weapons systems) but prefer the legitimacy shield the NPT frame provides.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, dual_use_technology_suppliers, beneficiary,
    institutional, generational, mobile, global).

% The IAEA and NPT review conferences produce technical analysis of compliance but operate under a mandate shaped by weapon states' interests. They observe but do not adjudicate whether the constraint is legitimate; they execute verification within the scope weapon states have granted them. Formally neutral; structurally constrained by their dependence on state cooperation.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, treaty_verification_machinery, observer,
    institutional, biographical, analytical, global).

% NGOs, humanitarian bodies, and progressive states advocate that Article VI creates a binding obligation to achieve nuclear disarmament and that the NPT's legitimacy depends on weapon states' demonstrable progress. They cite humanitarian law precedent (chemical weapons, biological weapons) to argue weapons prohibitions are binding once codified. They have no seat at the enforcement table but their reading is the one this constraint story instantiates.
narrative_ontology:constraint_stakeholder(npt_article_iv_vi_pairing__abolitionist, abolitionist_treaty_advocates, observer,
    powerless, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_article_iv_vi_pairing__abolitionist, weapon_states).
narrative_ontology:fixing_cost_class(npt_article_iv_vi_pairing__abolitionist, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates non-weapon states' restraint on horizontal proliferation in exchange for promised access to peaceful nuclear technology and disarmament progress. The coordination problem it solves (preventing weapons cascades) is real.
% TRANSFER_FUNCTION: Transfers sovereignty restraint from non-weapon states (foregoing weapons programs, accepting inspections) to weapon states (retaining arsenals, setting technology policy, collecting security rent). Simultaneously transfers dual-use technology export rents from non-weapon states' alternative enrichment paths to approved suppliers operating within the peaceful-use frame.
% ABSENT_VOICES: Humanitarian constituencies and future generations who would bear existential risk from weapons proliferation are structurally excluded from NPT governance. States outside the treaty (India, Pakistan, Israel, North Korea) would argue the regime is illegitimate precisely because it locks in weapon states' monopoly.
% DISAPPEARANCE_RATIONALE: If this constraint—the institutional reading that Article IV and VI are reciprocal but Article VI is non-binding—disappeared, non-weapon states would immediately pursue independent enrichment and reprocessing capacity, weapon states would face existential pressure to either disarm or explicitly repudiate their legal obligations, and the dual-use technology market would fragment into sanctioned and unsanctioned channels. The global nuclear order would reorganize.
% FOUNDING_PROBLEM: The 1968 NPT founding problem was preventing horizontal proliferation to additional states while offering peaceful technology access and a disarmament pathway. The founding coalition was Cold War weapon states seeking to lock in their monopoly while preserving legitimacy, non-aligned states seeking to avoid appearing aggressive, and suppliers seeking regulated markets.
% FOUNDING_PROBLEM_CORROBORATION: Weapon states claim disarmament is 'ongoing' and cite arms-control agreements (not disarmament). Non-weapon states' treaty-review statements, declassified memos from disarmament advocates, and humanitarian-law organizations all attest that the founding problem—preventing cascading proliferation through a shared disarmament commitment—is unresolved because weapon states have not disarmed. The TPNW's 2017 adoption by 122 states formally repudiates the founding coalition's framing as insufficient.
narrative_ontology:disappearance_verdict(npt_article_iv_vi_pairing__abolitionist, world_rearranges).
narrative_ontology:founding_problem_status(npt_article_iv_vi_pairing__abolitionist, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_article_iv_vi_pairing__abolitionist, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_article_iv_vi_pairing__abolitionist, 'none', 1).
narrative_ontology:epsilon_provenance(npt_article_iv_vi_pairing__abolitionist, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.45 to 0.78 over the interval, reflecting accumulating evidence that Article VI disarmament is not happening and weapon states' monopoly is institutionalizing. Theater ratio rises from 0.35 to 0.62 as review conferences and disarmament talks become increasingly performative—the machinery exists but produces no binding timelines or enforcement. Suppression requirement plateaus around 0.71 because the constraint's persistence depends on maintaining non-weapon states' belief in eventual disarmament (constant suppression of defection incentives) and on technological gatekeeping (constant exclusion of independent enrichment paths). The coordination function (preventing horizontal proliferation) is real; the extraction (weapon states collecting security rent and dual-use suppliers capturing technology markets) is substantial and growing. Accessibility collapse at 0.68 reflects that non-weapon states' alternatives are technically and politically constrained but not zero—withdrawal is costly but theoretically available, and the TPNW offers an alternative framing. Resistance at 0.72 reflects consistent pushback from humanitarian constituencies, non-aligned states, and TPNW signatories who explicitly reject the nonproliferation_primary reading.
 *
 * PERSPECTIVAL GAP:
 *   Weapon states would classify this constraint as rope or even mountain (disarmament as geopolitically impossible, not as their choice). Non-weapon states and humanitarian constituencies would classify it as snare (extraction defended by their own weakness). The abolitionist reading explicitly endorses the non-weapon-state perspective: Article VI binds weapon states, Article IV's legitimacy depends on disarmament progress, and the current institutional arrangement violates humanitarian law norms. The nonproliferation_primary reading (sibling) would classify the same structure as rope—a genuine coordination achievement that has prevented wider proliferation. The grand_bargain reading would classify it as tangled_rope but with an important difference: the grand_bargain reading holds that Article VI is enforceable, so the asymmetry is theoretically correctable if weapon states comply. The abolitionist reading holds that Article VI is enforced, weapon states have violated it, and the constraint persists only through institutional suppression of this violation.
 *
 * DIRECTIONALITY LOGIC:
 *   Weapon states occupy the beneficiary/agenda-setter seat with d near 0.0 (full beneficiary): they set the rules, retain arsenals, and collect security rent from the monopoly. Non-weapon states occupy the payer seat with d near 1.0 (full target): they accept restraint, submit inspections, and receive promises that do not materialize. Humanitarian constituencies and excluded voices sit at d near 1.0 as well—they would object if present but are structurally excluded from the enforcement machinery. Dual-use suppliers sit at d between 0.2-0.3 (partial beneficiary with constrained exit): they profit from the ambiguous reading but have alternative markets if the constraint were tightened. From the weapon-state seat, this looks like a legitimate coordination mechanism they built and maintain (their perception: rope). From the non-weapon-state seat, the same structure operates as asymmetric extraction defended by institutional power (their perception: snare/tangled_rope). The engine computes these per-seat divergences from the structural data.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing horizontal proliferation through disarmament reciprocity) is dead under the abolitionist reading: weapon states have not disarmed, non-weapon states have not cascaded, and the causal mechanism the regime was built on has broken. The constraint persists through theater and institutional inertia. Mandatrophy resolution requires either: (1) weapon states actually disarm (Article VI compliance), which would transform the constraint from tangled_rope to genuine rope; (2) non-weapon states withdraw or build alternative regimes (TPNW signatory path), which would fragment the constraint; or (3) explicit codification that Article VI is non-binding and Article IV stands alone (nonproliferation_primary reading victory), which would reclassify the constraint as pure coordination without disarmament obligation. The abolitionist reading positions mandatrophy as the current state—the mandate to disarm has become theatrical, the constraint persists despite mandate failure, and institutional reform is blocked by weapon-state veto.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI''s disarmament language a binding legal obligation or aspirational language without enforcement mechanism?',
    'International Court of Justice advisory opinion on treaty text interpretation under Vienna Convention; state-party amicus briefs on legislative history; treaty-review conference formal adjudication (unlikely given weapon-state veto).',
    'If binding: weapon states are in violation, abolitionist reading is structurally vindicated, constraint should reclassify toward snare with institutional suppression of violation. If aspirational: nonproliferation_primary reading prevails, constraint reclassifies toward pure rope (real coordination, no mandatrophy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, empirical, 'Legal status of Article VI disarmament obligation').

omega_variable(
    humanitarian_law_supremacy_over_article_iv,
    'Does the humanitarian weapons-prohibition norm (as codified in TPNW and chemical/biological precedent) legally supersede Article IV peaceful-use rights?',
    'State consent to TPNW interpretation in ICJ proceedings; national court rulings on conflicting treaty obligations; explicit negotiation at next NPT review conference on Article IV scope.',
    'If yes: Article IV''s dual-use export pathways are illegitimate, non-weapon states'' grievance is validated, constraint''s asymmetry is recognized. If no: Article IV access is independent of prohibition norms, dual-use suppliers retain legitimacy, constraint persists as tangled_rope defending status quo.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_law_supremacy_over_article_iv, conceptual, 'Whether humanitarian law supremacy constrains Article IV peaceful use').

omega_variable(
    weapon_state_compliance_mechanisms,
    'What verification and enforcement mechanisms exist to detect and respond to weapon-state non-compliance with Article VI?',
    'Forensic analysis of weapon stockpile trends, declassified strategic documents, IAEA mandate expansion proposals, treaty-review conference negotiation outcomes.',
    'If zero effective mechanisms: the constraint is purely theatrical and extractive, theater_ratio should rise further. If mechanisms exist but are unused: institutional suppression is the operative fact. If mechanisms are activated: constraint reclassifies toward genuine rope or scaffolds toward disarmament.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(weapon_state_compliance_mechanisms, empirical, 'Whether Article VI has verifiable, enforceable disarmament mechanisms').

omega_variable(
    tpnw_delegitimization_path,
    'Does the TPNW''s institutional existence and the 122-state endorsement constitute a formal repudiation of the abolitionist reading, or does it validate it by revealing the NPT''s inadequacy?',
    'Track TPNW entry-into-force, signature-to-ratification conversion rates, non-weapon-state coalition rhetoric in treaty review cycles, weapon-state countervailing institutional investments.',
    'If TPNW becomes the primary regime: NPT reclassifies as vestigial coordinate-mechanism with zero extraction rents (constraint dissolves). If NPT persists as primary: TPNW remains isolated, abolitionist reading gains rhetorical power but institutional suppression continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tpnw_delegitimization_path, empirical, 'Whether TPNW represents constraint-dissolving or constraint-reinforcing development').

omega_variable(
    dual_use_technology_distinguishability,
    'Is there a technical or institutional boundary between peaceful and military nuclear pathways, or is the distinction inherently ambiguous?',
    'Expert assessments from IAEA, National Academies, weapons-lab reviews; historical case studies (Iran, North Korea, Syria, Iraq) of enrichment-to-weapons escalation trajectories.',
    'If distinguishable and enforceable: Article IV can be legitimately bounded by dual-use constraints; extractiveness declines as non-weapon states gain genuine peaceful access. If inherently ambiguous: peaceful-use language is cover for dual-use export; extraction persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(dual_use_technology_distinguishability, empirical, 'Whether peaceful-use and military-use pathways are structurally separable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_article_iv_vi_pairing__abolitionist, 0, 54).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(npt__tr_t0, observed).
narrative_ontology:measurement(npt__tr_t9, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 9, 0.4).
narrative_ontology:measurement_basis(npt__tr_t9, observed).
narrative_ontology:measurement(npt__tr_t18, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 18, 0.48).
narrative_ontology:measurement_basis(npt__tr_t18, observed).
narrative_ontology:measurement(npt__tr_t27, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 27, 0.55).
narrative_ontology:measurement_basis(npt__tr_t27, observed).
narrative_ontology:measurement(npt__tr_t36, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 36, 0.59).
narrative_ontology:measurement_basis(npt__tr_t36, observed).
narrative_ontology:measurement(npt__tr_t45, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 45, 0.61).
narrative_ontology:measurement_basis(npt__tr_t45, observed).
narrative_ontology:measurement(npt__tr_t54, npt_article_iv_vi_pairing__abolitionist, theater_ratio, 54, 0.62).
narrative_ontology:measurement_basis(npt__tr_t54, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(npt__be_t0, observed).
narrative_ontology:measurement(npt__be_t9, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(npt__be_t9, observed).
narrative_ontology:measurement(npt__be_t18, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 18, 0.61).
narrative_ontology:measurement_basis(npt__be_t18, observed).
narrative_ontology:measurement(npt__be_t27, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 27, 0.7).
narrative_ontology:measurement_basis(npt__be_t27, observed).
narrative_ontology:measurement(npt__be_t36, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 36, 0.75).
narrative_ontology:measurement_basis(npt__be_t36, observed).
narrative_ontology:measurement(npt__be_t45, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 45, 0.77).
narrative_ontology:measurement_basis(npt__be_t45, observed).
narrative_ontology:measurement(npt__be_t54, npt_article_iv_vi_pairing__abolitionist, base_extractiveness, 54, 0.78).
narrative_ontology:measurement_basis(npt__be_t54, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(npt__su_t0, observed).
narrative_ontology:measurement(npt__su_t9, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 9, 0.61).
narrative_ontology:measurement_basis(npt__su_t9, observed).
narrative_ontology:measurement(npt__su_t18, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 18, 0.65).
narrative_ontology:measurement_basis(npt__su_t18, observed).
narrative_ontology:measurement(npt__su_t27, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 27, 0.68).
narrative_ontology:measurement_basis(npt__su_t27, observed).
narrative_ontology:measurement(npt__su_t36, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 36, 0.7).
narrative_ontology:measurement_basis(npt__su_t36, observed).
narrative_ontology:measurement(npt__su_t45, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 45, 0.71).
narrative_ontology:measurement_basis(npt__su_t45, observed).
narrative_ontology:measurement(npt__su_t54, npt_article_iv_vi_pairing__abolitionist, suppression_requirement, 54, 0.71).
narrative_ontology:measurement_basis(npt__su_t54, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_article_iv_vi_pairing__abolitionist, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_article_iv_vi_pairing__abolitionist, 0.12).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__nonproliferation_primary).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, npt_article_iv_vi_pairing__grand_bargain).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, tpnw_humanitarian_weapons_prohibition).
narrative_ontology:affects_constraint(npt_article_iv_vi_pairing__abolitionist, iaea_verification_legitimacy).

% DUAL FORMULATION NOTE:
% The NPT Article IV-VI pairing is a single kernel with three distinct structural readings: abolitionist (this story), nonproliferation_primary, and grand_bargain. Each reading produces a different constraint classification and different beneficiary/victim structure from the same text. The kernel's ambiguity is irreducible in the text itself; the readings diverge on authority grounding (humanitarian law vs. weapon-state security interest), binding status of Article VI, and whether Article IV is constrained by prohibition norms. Each reading should be authored as a separate constraint story with its own epsilon and stakeholder structure. The network edges above link this reading to its siblings and to downstream constraints (TPNW, IAEA authority) that are affected by which reading dominates institutional practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, powerless, 0.95).
constraint_indexing:directionality_override(npt_article_iv_vi_pairing__abolitionist, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
