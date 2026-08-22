% ============================================================================
% CONSTRAINT STORY: udhr_authority__aspirational_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_udhr_authority__aspirational_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: udhr_authority__aspirational_sovereignty_reading
 *   human_readable: UDHR as Moral Guidance — Consent-Gated Obligation (Aspirational-Sovereignty Reading)
 *   domain: international_law/political_philosophy/human_rights_doctrine
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the udhr_authority kernel: the
 *   aspirational-sovereignty reading, under which the Universal Declaration
 *   provides a common moral standard while binding obligation arises only
 *   through state consent expressed by treaty ratification or accepted
 *   custom. The standing arrangement under contest — the ε referent — is that
 *   consent-gated regime as it actually operates: a proclamation no organ can
 *   enforce against an unwilling state, tribunals without coercive power
 *   absent ratification, and every government free to invoke or ignore the
 *   text. Assessed by this reading's own lights, the arrangement extracts
 *   very little from state autonomy (ε 0.22): the consent gate is, for this
 *   reading, the legitimate constitutional structure of international law,
 *   not a defect. The claim/metric gap is deliberate and independent:
 *   claimed_type is rope (genuine coordination — shared evaluative vocabulary
 *   and standard-setting — with minimal coercive overhead and open
 *   alternatives), while the metrics report what descriptively obtains,
 *   including a theater ratio that has climbed to half of all activity.
 *   Sibling readings (binding_universalism_reading,
 *   customary_emergence_reading) are other constraints in the same family,
 *   linked via network.affects_constraints; they are not folded into this
 *   story's ε, per the one-reading-one-constraint rule.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary beneficiary (institutional/arbitrage) — collectively proclaim the standard, individually decide what binds them, and lose nothing by refusal
 *   - permanent_five_great_powers: Concentrated beneficiary (institutional/arbitrage) — the gate shields precisely the conduct most likely to be targeted by non-consensual enforcement, and their weight shapes which obligations ever reach agreement
 *   - small_and_middle_powers: Distributed beneficiary (organized/constrained) — rely on the same gate as protection against imposition by majority vote or tribunal fiat
 *   - individual_rights_claimants: Excluded seat (powerless/trapped) — announced as 'everyone' in the text but holding no procedural venue absent their state's consent
 *   - human_rights_advocacy_networks: Excluded seat (organized/constrained) — press binding interpretations whose strongest outputs end in recommendation rather than order
 *   - un_human_rights_machinery: Analytical observer (institutional/analytical) — reviews, reports, and recommends within the space the consent gate leaves open
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(udhr_authority__aspirational_sovereignty_reading, 0.22).
domain_priors:suppression_score(udhr_authority__aspirational_sovereignty_reading, 0.3).
domain_priors:theater_ratio(udhr_authority__aspirational_sovereignty_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(udhr_authority__aspirational_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(udhr_authority__aspirational_sovereignty_reading, rope).
narrative_ontology:human_readable(udhr_authority__aspirational_sovereignty_reading, "UDHR as Moral Guidance — Consent-Gated Obligation (Aspirational-Sovereignty Reading)").
narrative_ontology:topic_domain(udhr_authority__aspirational_sovereignty_reading, "international_law/political_philosophy/human_rights_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(udhr_authority__aspirational_sovereignty_reading, '3f6e5524-b938-4248-8f6a-9626386f1ac5').
narrative_ontology:cs_kernel_codification('3f6e5524-b938-4248-8f6a-9626386f1ac5', fixed_text).
narrative_ontology:cs_authority_grounding('3f6e5524-b938-4248-8f6a-9626386f1ac5', diffuse_epistemic).
narrative_ontology:cs_reading_relation('3f6e5524-b938-4248-8f6a-9626386f1ac5', udhr_authority__binding_universalism_reading, coexists_with).
narrative_ontology:cs_reading_relation('3f6e5524-b938-4248-8f6a-9626386f1ac5', udhr_authority__customary_emergence_reading, coexists_with).
narrative_ontology:cs_axiom('3f6e5524-b938-4248-8f6a-9626386f1ac5', foundational, binding_obligation_requires_state_consent).
narrative_ontology:cs_axiom_status(binding_obligation_requires_state_consent, holdable).
narrative_ontology:cs_axiom_grounding('3f6e5524-b938-4248-8f6a-9626386f1ac5', binding_obligation_requires_state_consent, conventional).
narrative_ontology:cs_axiom('3f6e5524-b938-4248-8f6a-9626386f1ac5', foundational, declaration_authority_is_moral_not_juridical).
narrative_ontology:cs_axiom_status(declaration_authority_is_moral_not_juridical, holdable).
narrative_ontology:cs_axiom_grounding('3f6e5524-b938-4248-8f6a-9626386f1ac5', declaration_authority_is_moral_not_juridical, deontological).
narrative_ontology:cs_reference_frame('3f6e5524-b938-4248-8f6a-9626386f1ac5', consent_gated_aspirational_standard).
narrative_ontology:cs_drift_state('3f6e5524-b938-4248-8f6a-9626386f1ac5', contemporary_custom_formation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3f6e5524-b938-4248-8f6a-9626386f1ac5', '').
narrative_ontology:cs_kernel_id(udhr_authority__aspirational_sovereignty_reading, udhr_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, sovereign_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, permanent_five_great_powers).
narrative_ontology:constraint_beneficiary(udhr_authority__aspirational_sovereignty_reading, small_and_middle_powers).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, state_consent_source_doctrine).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, sovereign_equality_principle).
narrative_ontology:constraint_vindicates(udhr_authority__aspirational_sovereignty_reading, unga_resolution_non_binding_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively proclaim and periodically re-proclaim the Declaration through General Assembly resolution; each government separately decides which instruments to sign, ratify, or ignore. The arrangement leaves every government's domestic jurisdiction intact: no organ adjudicates Declaration violations against an unwilling state, and each government gains a common moral vocabulary it may invoke or decline at will. Withholding ratification or entering reservations carries no Declaration-level penalty, so disengagement is always available.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, sovereign_states, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(udhr_authority__aspirational_sovereignty_reading, sovereign_states, agenda_setter).

% Veto-holding Security Council members and the largest economies. The consent gate costs them least and shields them most: their conduct abroad is the most likely target of any non-consensual enforcement scheme, and their diplomatic weight shapes which obligations ever reach agreement text. Several have declined to ratify major human rights covenants or entered sweeping reservations while retaining full standing and voice in the system.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, permanent_five_great_powers, beneficiary,
    institutional, generational, arbitrage, global).

% Rely on the same gate as a shield: it guarantees that larger states or majorities cannot impose obligations on them by vote or tribunal order. They participate actively in standard-setting conferences and ratify selectively, trading commitments for concessions. Their protection depends on the gate holding for everyone, including the great powers, which gives them a stake in defending a rule that chiefly benefits their larger counterparts.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, small_and_middle_powers, beneficiary,
    organized, generational, constrained, regional).

% Persons whose rights the Declaration announces. The text addresses them as 'everyone', but they hold no procedural seat: they cannot bring claims before the principal international organs, and where their own state has not consented to a complaints procedure they have no venue at all. Their recourse is domestic politics, regional courts where their state permits them, or persuasion — none of which they control.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, individual_rights_claimants, excluded,
    powerless, biographical, trapped, national).

% Non-governmental organizations, scholarly movements, and campaign coalitions that press governments to treat the Declaration's provisions as already binding. They operate through reporting, public shaming, and litigation strategies that borrow the Declaration's authority; because obligation requires consent, their strongest arguments terminate in recommendation and dialogue rather than order, and they cannot compel a hearing anywhere.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, human_rights_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% The Human Rights Council, treaty bodies, special procedures, and the Office of the High Commissioner. They examine country situations, issue observations, and recommend; their output is documentation and dialogue, not orders. Their continued existence depends on member states funding and cooperating with the machinery that the consent-based structure leaves room for.
narrative_ontology:constraint_stakeholder(udhr_authority__aspirational_sovereignty_reading, un_human_rights_machinery, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(udhr_authority__aspirational_sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(udhr_authority__aspirational_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a common moral vocabulary and baseline standard for evaluating state conduct across radically different legal systems — enabling diplomatic discourse, treaty preambles, domestic constitution-building, and post-conflict reconstruction to proceed from shared reference points in a setting where coercion between states is unavailable.
% TRANSFER_FUNCTION: Moves almost nothing material. What circulates is reputational and rhetorical currency: governments gain legitimacy by invoking the standard, critics gain leverage by invoking it against them, and newly formed states gain a ready-made template for bills of rights — but no wealth, jurisdiction, or decision-authority moves except through separately consented instruments.
% ABSENT_VOICES: Individual rights claimants would object loudest: the Declaration speaks of 'everyone' yet the arrangement admits only states as speakers, and the people whose rights are announced have no seat, no standing, and no venue. Advocacy networks hold partial voice through consultative arrangements but none on the bindingness question itself, which states reserve to themselves.
% DISAPPEARANCE_RATIONALE: If the Declaration-as-shared-standard vanished overnight, the postwar rights architecture would lose its anchoring text: the covenants' preambles, dozens of national constitutions, regional conventions, and decades of diplomatic practice all cite it as their common reference. Standard-setting would restart from fragmentation, advocacy would lose its shared vocabulary, and the consent-gate settlement itself — the thing this reading defends — would lose the object it gates.
% FOUNDING_PROBLEM: After 1939–1945 demonstrated that sovereignty had shielded atrocity, the drafters sought a common articulation of human rights that could command near-universal assent without compelling any state: a 'common standard of achievement for all peoples and nations', with binding legal form deferred to covenants that states would adopt only through consent.
% FOUNDING_PROBLEM_CORROBORATION: The genealogy is corroborated from outside the beneficiary set by diplomatic-history scholarship on the Third Committee and Commission on Human Rights records, which attests the drafters deliberately chose declaratory form knowing enforcement required later consent. On STATUS the parties divide: sovereigntist governments attest the guidance function remains live amid continuing violations, while universalist jurists and advocacy scholarship attest the articulation task completed in 1948–1966 and that persistence at the Declaration level is now largely ceremonial — no neutral arbiter exists, which is itself the signal recorded here.
narrative_ontology:disappearance_verdict(udhr_authority__aspirational_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(udhr_authority__aspirational_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(udhr_authority__aspirational_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(udhr_authority__aspirational_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(udhr_authority__aspirational_sovereignty_reading, 0.22, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(udhr_authority__aspirational_sovereignty_reading_tests).
:- end_tests(udhr_authority__aspirational_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.22 at interval end) because the arrangement transfers nothing material: no wealth, jurisdiction, or decision-authority moves absent separate consent, and the reading indexes ε to the consent-gated arrangement as legitimate. Suppression (0.30) is a raw structural property, unscaled by power or scope: the gate blocks non-consensual enforcement routes (custom claims, universalist adjudication) but coerces no one — withholding consent carries no sanction. Accessibility collapse is low (0.25) because alternatives remain fully open — bilateral treaties, regional conventions, customary acceptance — optionality being the design, not a residue. Resistance (0.50) reflects sustained pushback from universalist scholarship, advocacy campaigns, and progressive jurisprudence contesting the gate, met by periodic state reaffirmations of sovereignty. Theater (0.50) traces the migration of standard-setting into treaty bodies after 1966: what remains at the Declaration level is increasingly commemorative resolution and rhetorical deployment alongside still-real operative uses (constitutional drafting, judicial citation). The measurement series run on ONE shared eight-point grid (1948–2025) with both tracked metrics authored at every point; a suppression_requirement series is deliberately omitted because the enforcement picture is static by construction — the arrangement's defining feature is the absence of coercive machinery to intensify or decay, and that stasis is carried by the base_properties.suppression scalar.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the permanent-five seat the gate preserves discretion those governments would irretrievably lose under non-consensual enforcement, and their exit options (selective ratification, sweeping reservations) sit at the arbitrage end. From the small-and-middle-power seat the identical gate is a protective shield — the guarantee that obligations cannot be imposed on them by stronger coalitions — which makes them invested in the gate holding for everyone, including rivals. From the individual-rights-claimant seat the same arrangement presents as a closed door: a moral promise pronounced in their name with no venue attached, from a position of no exit at all. From the advocacy-network seat the gate reads as the mechanism that converts their strongest legal arguments into recommendations. The engine computes these per-seat classifications from the power and exit data; this story authors the structure and does not adjudicate the divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: sovereign_states (with the P5 and small-power sub-seats as its concentrated and distributed instances) derive directionality near the beneficiary end — the arrangement subsidizes their autonomy. No victim class is declared, and this is a substantive finding, not an omission: the arrangement transfers nothing from anyone, so the people most affected by its limits (individual claimants) are modeled as EXCLUDED seats rather than victims — they bear an absence of provision, not a transfer through the structure, and converting that absence into extraction would import the sibling readings' geometry into this one. The residual extraction the reading does register (unconsented reputational pressure accompanying the text's growing moral authority) lands on no fixed seat — it dissipates into discourse — which is why gain_flow is authored as diffuse after checking every named seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a normative vacuum in which sovereignty shielded atrocity — was substantively addressed twice: by the 1948 articulation itself and by the 1966 covenants that converted core provisions into consent-based law. What persists at the Declaration level is maintained by active state preference (every government benefits from the gate) rather than by nobody-caring inertia, which distinguishes this from a degraded piton even as theater climbs. The classification guards against two mislabelings: calling the arrangement a snare would require identifiable victims and a transfer the structure lacks — the unenforceability gap is an absence, not a taking; calling it a mountain would require naturality the consent doctrine lacks — it is an enacted, revisable choice, repeatedly reaffirmed and contestable. The rising theater ratio is documented honestly as a symptom worth watching (see the guidance_function_vitality omega) without letting theatricality alone drive reclassification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    udhr_kernel_reading_contest,
    'This constraint is one reading (aspirational_sovereignty_reading) of the udhr_authority kernel. What structurally changes if a sibling reading governs instead?',
    'Doctrinal development: binding_universalism_reading would make rights justiciable against states regardless of consent (epsilon on state autonomy rises sharply, victims appear); customary_emergence_reading would relocate obligation-formation from the consent gate to state practice plus opinio juris (the gate survives only for non-customary provisions). Track ICJ and regional-court holdings, treaty-body assertions of competence, and state objection patterns.',
    'Under binding universalism the arrangement recomputes as substantially extractive toward non-consenting states; under customary emergence the consent gate narrows to a residual rule and this story''s epsilon applies to a shrinking band of provisions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(udhr_kernel_reading_contest, conceptual, 'Committer structure: which reading of the UDHR-authority kernel governs determines the entire beneficiary/victim geometry.').

omega_variable(
    customary_absorption_trajectory,
    'Is the consent gate eroding as specific UDHR provisions (torture prohibition, slavery, non-discrimination core) harden into customary international law or jus cogens?',
    'Longitudinal coding of judicial citations to the Declaration as law versus as guidance, International Law Commission work on identification of custom, and state responses (persistent objection, acquiescence) to custom claims.',
    'If absorption continues, the aspirational reading''s scope contracts provision-by-provision and measured extraction on non-consenting states rises within the absorbed band; if absorption stalls, the gate holds across the board.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_absorption_trajectory, empirical, 'Whether portions of the Declaration have exited the consent gate through custom formation.').

omega_variable(
    reputational_pressure_extraction_status,
    'Does the Declaration''s moral authority impose legitimacy costs on non-consenting states (a form of extraction without consent), or is reputational and rhetorical pressure categorically outside extraction?',
    'Conceptual analysis separating coercive transfer from persuasive discourse, combined with empirical study of whether naming-and-shaming produces compliance shifts that states would not otherwise choose.',
    'If reputational pressure counts as extraction, epsilon rises materially despite the absence of legal compulsion and the beneficiary/victim geometry gains a diffuse payer class; if not, the current low reading-indexed epsilon stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reputational_pressure_extraction_status, conceptual, 'Whether unconsented moral pressure is extraction or ordinary discourse.').

omega_variable(
    guidance_function_vitality,
    'Does the moral-guidance function still operate, or has the arrangement become predominantly ceremonial within this reading?',
    'Count operative uses (domestic constitutional drafting, judicial citation, treaty-preamble reliance, new-state bill-of-rights modeling) against ceremonial invocations (commemorative resolutions, anniversary events, rhetorical speech-making) across successive decades.',
    'If operative uses continue declining along the theater trajectory, the arrangement drifts toward inertial persistence within this reading; if operative uses stabilize, the coordination function remains live and the rope claim holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(guidance_function_vitality, empirical, 'Whether the guidance function is operative or performed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(udhr_authority__aspirational_sovereignty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udhr_aspirational_sov_tr_t1948, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1948, 0.15).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t1948, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t1955, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1955, 0.2).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t1955, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t1966, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1966, 0.24).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t1966, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t1977, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1977, 0.33).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t1977, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t1989, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 1989, 0.36).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t1989, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t2001, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2001, 0.41).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t2001, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t2013, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2013, 0.46).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t2013, observed).
narrative_ontology:measurement(udhr_aspirational_sov_tr_t2025, udhr_authority__aspirational_sovereignty_reading, theater_ratio, 2025, 0.5).
narrative_ontology:measurement_basis(udhr_aspirational_sov_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(udhr_aspirational_sov_be_t1948, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1948, 0.1).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t1948, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t1955, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1955, 0.12).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t1955, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t1966, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1966, 0.14).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t1966, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t1977, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1977, 0.16).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t1977, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t1989, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 1989, 0.17).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t1989, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t2001, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2001, 0.18).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t2001, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t2013, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2013, 0.2).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t2013, observed).
narrative_ontology:measurement(udhr_aspirational_sov_be_t2025, udhr_authority__aspirational_sovereignty_reading, base_extractiveness, 2025, 0.22).
narrative_ontology:measurement_basis(udhr_aspirational_sov_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(udhr_authority__aspirational_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(udhr_authority__aspirational_sovereignty_reading, information_standard).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, binding_universalism_reading).
narrative_ontology:affects_constraint(udhr_authority__aspirational_sovereignty_reading, customary_emergence_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the ε-invariance principle: the colloquial label 'UDHR authority' covers three structurally distinct claims that the kernel contest separates. This story (aspirational_sovereignty_reading) carries low reading-indexed ε on state autonomy and no victim class; binding_universalism_reading carries high ε on state autonomy with individuals as rights-holders against states; customary_emergence_reading carries a migrating ε keyed to which provisions have crossed into custom. The upstream story in empirical-confidence terms is this one (the declaratory status of the 1948 text is the settled historical baseline), and both siblings cite or react against it: universalist advocacy argues FROM the Declaration's moral authority, and custom arguments must defeat the consent gate this reading codifies. All three files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
