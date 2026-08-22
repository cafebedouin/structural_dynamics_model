% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Nation-to-Nation Treaty Reading: Indigenous Sovereignty & Ongoing Consent
 *   domain: legal/constitutional/indigenous_law
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested kernel called
 *   'historical treaty substrate'—specifically, the nation-to-nation reading,
 *   which frames treaties as binding international law instruments between
 *   sovereign equals that require ongoing consent for territorial and
 *   resource changes. Under this reading, Indigenous nations are not
 *   subordinate parties whose rights were extinguished by historical
 *   treaties, but co-equal sovereigns whose consent authority persists and is
 *   protected by international law principles (pacta sunt servanda, free
 *   prior informed consent). The settler state is constrained by its own
 *   treaty obligations and by international legal accountability. Unilateral
 *   resource extraction without Indigenous consent becomes a treaty
 *   violation. This reading diverges sharply from the extinguishment reading
 *   (which treats treaties as completed property sales) and the stewardship
 *   reading (which treats them as relational coexistence pacts without
 *   consent-based sovereignty). The nation-to-nation reading's authority
 *   derives from post-1945 international treaty law, UN frameworks (UNDRIP,
 *   ILO 169), and appellate court adoption in several settler states. The
 *   measurement series tracks the extractiveness and enforcement burden
 *   increasing over the 1975–2026 interval as resource pressure intensified
 *   and Indigenous nations deployed the reading to contest unilateral
 *   extraction, creating friction with settler-state legislatures and
 *   extractive industries.
 *
 * KEY AGENTS:
 *   - Indigenous nations: organized sovereigns with civilizational time horizon and identity-locked exit (constrained by settler legal systems but bound by cultural and territorial continuity); hold consent rights under this reading
 *   - Settler state: institutional agenda-setter constrained by treaty obligations it formally recognized; caught between international law accountability and domestic political pressure
 *   - Extractive industries: powerful institutional actors facing consent friction and deal costs; constrained because the territorial resource base is fixed and legal access requires Indigenous agreement
 *   - International treaty bodies: institutional observers interpreting and adjudicating treaty obligations across jurisdictions; carry structural authority to reinforce or undermine the nation-to-nation reading
 *   - Domestic courts: institutional arbiters of treaty meaning; some (Canada, Australia post-2000s) adopt the nation-to-nation reading; others defer to legislative sovereignty, creating seat-level divergence
 *   - Settler legislatures: institutional agenda-setters with formal legislative supremacy but constrained by international law obligation and court rulings; mobile exit exists in principle (treaty denunciation) but carries geopolitical cost
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.68).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.71).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, tangled_rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Nation-to-Nation Treaty Reading: Indigenous Sovereignty & Ongoing Consent").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal/constitutional/indigenous_law").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '2f3138aa-e27e-4c24-a9d7-2430ed38fd19').
narrative_ontology:cs_kernel_codification('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', fixed_text).
narrative_ontology:cs_authority_grounding('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', lineage).
narrative_ontology:cs_interpretation_layer_present('2f3138aa-e27e-4c24-a9d7-2430ed38fd19').
narrative_ontology:cs_reading_relation('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', foundational, indigenous_nations_are_sovereigns).
narrative_ontology:cs_axiom_status(indigenous_nations_are_sovereigns, holdable).
narrative_ontology:cs_axiom_grounding('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', indigenous_nations_are_sovereigns, deontological).
narrative_ontology:cs_axiom('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', foundational, ongoing_consent_binding_in_present).
narrative_ontology:cs_axiom_status(ongoing_consent_binding_in_present, holdable).
narrative_ontology:cs_axiom_grounding('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', ongoing_consent_binding_in_present, empirically_contingent).
narrative_ontology:cs_axiom('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', secondary, pacta_sunt_servanda_unilateral_constraint).
narrative_ontology:cs_axiom_status(pacta_sunt_servanda_unilateral_constraint, holdable).
narrative_ontology:cs_axiom_grounding('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', pacta_sunt_servanda_unilateral_constraint, conventional).
narrative_ontology:cs_reference_frame('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', post_1945_international_treaty_law_framework).
narrative_ontology:cs_drift_state('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', contemporary_2026, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f3138aa-e27e-4c24-a9d7-2430ed38fd19', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, extractive_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned as co-equal sovereigns under this reading; hold consent rights over territorial changes and resource extraction per treaty terms and international law principles. Simultaneously bear costs when the settler state fails to honor ongoing consent obligations, when unilateral extraction proceeds as treaty violation, and when enforcement requires legal mobilization and institutional contestation. Identity-locked because departure from the nation-to-nation framework dissolves their legal standing as sovereigns and returns them to subordinate status under extinguishment or stewardship readings.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, payer).

% Administers and enforces the treaty framework while simultaneously constrained by it. Under this reading, holds formal obligation to seek ongoing consent for territorial changes and resource extraction. Carries cost when international law obligations collide with domestic political pressure for resource development or territorial claims. Constrained rather than mobile because exit would require renouncing international treaty law status and domestic constitutional supremacy doctrines that cite pacta sunt servanda.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state, agenda_setter,
    institutional, civilizational, constrained, national).
narrative_ontology:stakeholder_secondary_role(historical_treaty_substrate__nation_to_nation_reading, settler_state, payer).

% Face consent and consultation requirements under this reading that were absent or minimal under extinguishment reading. Resource access requires Indigenous nation agreement rather than settler-state permission alone. Constrained exit because the territorial resource base is fixed; operating within this legal frame means higher transaction costs and deal friction but remains the only path to resource access.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, extractive_industries, payer,
    powerful, biographical, constrained, regional).

% Interpret and adjudicate treaty obligations under UN frameworks (UNDRIP, ILO 169, treaty law conventions). Analytical seat: they witness the constraint's operation across multiple settler states and Indigenous nations, document compliance patterns, and issue findings that either reinforce or undermine the nation-to-nation reading's practical authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_treaty_bodies, observer,
    institutional, generational, analytical, global).

% Enforce or undermine the nation-to-nation reading through constitutional interpretation and treaty law rulings. Some domestic courts adopt the reading (Canada, New Zealand, Australia post-2000s); others invoke extinguishment or stewardship to limit Indigenous consent rights. Constrained because international treaty law obligations create downward pressure on domestic jurisprudence even when political pressure runs the other way.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, domestic_courts, agenda_setter,
    institutional, generational, constrained, national).

% Formally subject to treaty law and court rulings but retain legislative capacity to override or redefine treaties (legislative supremacy doctrine, though weakened). Actual exercise of that capacity triggers international law violations and diplomatic costs. Mobile exit exists in principle (unilateral denunciation) but carries substantial geopolitical cost, making the constraint binding in practice despite formal legislative authority.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_colonial_legislatures, agenda_setter,
    institutional, biographical, mobile, national).

% Resource-dependent settler populations (extraction workers, agricultural settlers on treaty lands) are structurally excluded from consent consultation. They would object to resource restrictions and territorial recognition but carry no formal voice in the nation-to-nation framing. Their objections are channeled through political pressure on legislatures and executives.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, excluded_settler_populations, excluded,
    moderate, biographical, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:fixing_cost_class(historical_treaty_substrate__nation_to_nation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable legal framework for coexistence between settler states and Indigenous nations by replacing unilateral territorial appropriation with bilateral treaty obligation. Solves the problem: how do two sovereigns share territory without one absorbing the other? Under this reading, the answer is reciprocal consent and ongoing negotiation binding on both parties.
% TRANSFER_FUNCTION: Transfers consent rights and refusal authority from the settler state alone to a joint decision structure requiring Indigenous nation agreement. Moves legitimacy: unilateral resource extraction becomes illegal; settler-state-permitted extraction becomes a treaty violation if Indigenous consent is withheld. Moves costs: extractive industries face new transaction costs and deal friction; Indigenous nations gain leverage but must exercise it, bearing organizational and opportunity costs.
% ABSENT_VOICES: Non-Indigenous settler populations economically dependent on resource extraction are excluded from the consent framework. Colonial-era settler interests (land speculators, agricultural expansionists) are also excluded. These populations would argue for resource access without Indigenous veto but are kept out by the same sovereign equality premise that includes Indigenous nations.
% DISAPPEARANCE_RATIONALE: If this reading's constraint disappeared and reverted entirely to settler-state unilateral authority, resource development would accelerate dramatically on treaty territories, Indigenous nation consent authority would evaporate, and the international legal status of settler states would shift—from treaty-bound sovereigns to treaty-violating powers. Territorial governance, resource economics, and geopolitical standing would reorganize around unconstrained settler sovereignty.
% FOUNDING_PROBLEM: Post-1945 decolonization and international law development elevated treaty law as the legitimate basis for state relations. The founding problem solved by this reading: how to square settler-colonial resource appropriation with the international legal principle that treaties bind all sovereign signatories equally, with no unilateral termination by the stronger party.
% FOUNDING_PROBLEM_CORROBORATION: International treaty bodies (UN Permanent Forum on Indigenous Issues, treaty monitoring committees) consistently affirm that ongoing consent is required under pacta sunt servanda. Appellate courts in Canada, Australia, and New Zealand have adopted variants of this reading in major cases. Independent human-rights organizations document treaty violations where extractive projects proceed without Indigenous consent. Corroboration comes from outside the Indigenous beneficiary set: international law scholars, court observers, and geopolitical analysts treating treaty obligation as binding.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claim is tangled_rope because this reading coordinates genuine conflict-reduction (a bilateral consent framework replaces zero-sum appropriation) while simultaneously creating asymmetric extraction: Indigenous nations are positioned as co-equal sovereigns yet bear costs when the settler state violates its own treaty obligations, when enforcement requires institutional mobilization, and when extractive industries deploy legal and political pressure to override consent. Extractiveness measures at 0.68 (high, rising from 0.42 in 1975) because the constraint imposes real costs on powerful extractive interests and settler-state budgets (consultation expenses, deal friction, foregone projects) while Indigenous nations must continually defend and re-assert their consent authority against erosion. Suppression measures at 0.71 (high, rising from 0.38) because enforcement requires active legal and political contestation: courts must rule repeatedly, international bodies must monitor, Indigenous nations must litigate and organize to block projects. Theater_ratio measures at 0.42 (moderate-high, rising from 0.15) because much consultation activity is performative—consultation occurs after project approval is already political fait accompli, Indigenous objections are overridden on 'national interest' grounds, and the settler state performs treaty compliance while evading its substance. Accessibility_collapse at 0.58 (moderate) because alternatives to the nation-to-nation reading remain live: courts can invoke extinguishment or stewardship, legislatures can signal unilateral treaty redefinition, and resource pressure creates ongoing pressure to contract the reading's scope. Resistance at 0.72 (high, substantial) because Indigenous nations mount real, organized resistance; extractive industries lobby aggressively; settler legislatures push back against court rulings. The measurement series shows all three metrics rising over the 51-year interval, indicating that as international law and court rulings strengthened the nation-to-nation reading, settler states increased enforcement costs (suppression) and Indigenous nations increased legal mobilization (resistance), while extraction continued to rise and performative consultation expanded (theater ratio). This is the signature of a tangled_rope under institutional contestation: the coordination function (bilateral conflict-reduction) persists, but so does the extraction and enforcement cost.
 *
 * PERSPECTIVAL GAP:
 *   Seat divergence is substantial and structural. From the settler-state institutional seat, the nation-to-nation reading is an unwelcome legal constraint that limits resource development and creates international law accountability; from the settler legislature, it is a constraint that can be formally acknowledged while being operationally evaded (consultation theater); from the extractive-industry seat, it is pure overhead—a regulatory friction that reduces project economics and increases deal complexity. From the Indigenous-nation seat, the reading is simultaneously a liberation framework (restoring sovereignty authority) and a burden (requiring continual legal contestation to enforce it). The international observer seat sees the reading as an articulation of universal treaty law principles that applies unevenly depending on settler-state judicial and political will. The engine will compute different d values for each seat from the structural data: Indigenous nations will compute as partial targets (yes, they benefit from sovereign recognition, but they pay the enforcement cost and bear the burden of contestation); the settler state will compute as partial beneficiary (treaty law shields it from unilateral Indigenous action) and partial target (international accountability); extractive industries will compute as targets (consent costs); settler legislatures will compute as targets (international legal pressure restricts unilateral action). This is not a failure of the structural data; it is the point: a genuine tangled_rope shows different seats seeing different types, and the engine's per-seat computation captures that asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations enter the beneficiary set because the reading restores their sovereigns status and consent authority—under the extinguishment reading they have no such authority; under this reading they do. They also appear in the victims set because enforcement is contested and costly: extractive projects proceed despite Indigenous objection; legislatures override consent; courts sometimes rule against Indigenous nations; and each contestation requires Indigenous nations to mobilize and litigate. The dual positioning is not a data error; it is the structure of the reading: it benefits Indigenous nations by granting sovereignty but costs them by forcing continuous institutional contestation to defend that grant against erosion. Settler state directionality is asymmetric: it benefits from treaty law's legitimacy as a sovereign actor (treaty law binds all sovereigns equally, which protects the settler state's own treaty interests and international standing) but is targeted by the obligation to recognize Indigenous consent (limiting unilateral resource extraction). Extractive industries are clearly targeted: they pay consultation costs, deal friction, and foregone projects when Indigenous nations withhold consent. The directionality logic flows from the reading's foundational axiom: that Indigenous nations are sovereigns whose consent is binding. This axiom creates structural winners (Indigenous nations gain authority) and structural losers (extractive interests face friction) and creates a middle case for the settler state (gains international legitimacy, loses unilateral resource authority).
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (the founding mandate has outlived its function but the structure persists through inertia) does NOT apply to this reading under current conditions. The founding problem—how to square international treaty law with settler-colonial appropriation—remains live: resource extraction continues to press against Indigenous consent; international law bodies continue to monitor and document violations; courts continue to rule on treaty interpretation. The reading is not yet Theater-only. However, the rising theater_ratio (0.15 to 0.42 over the interval) signals an emerging mandatrophy risk: if consultation becomes systematically performative (Indigenous voices heard but projects approved regardless), and if international legal accountability is routinely deflected, the reading's mandate—to establish binding Indigenous consent—could hollow out while the institutional structures persist. The reading is at the inflection point where mandatrophy becomes a risk rather than an actualized condition. This is why the commentaries and court cases increasingly invoke 'free prior informed consent' language as a corrective: to prevent the reading from degrading into pure theater while formally preserving the sovereignty framing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_authentication_ambiguity,
    'What institutional authority grounds the nation-to-nation reading as a binding interpretation of historical treaties? International law, settler-state courts, Indigenous nations'' own jurisprudence, or some combination?',
    'Empirical: compare treaty interpretations across international bodies (UN Permanent Forum, treaty monitoring committees), settler-state appellate courts, and Indigenous nation legal systems to identify which authorities converge on the nation-to-nation reading and which invoke alternatives.',
    'If authority is distributed (each authority produces different readings), the nation-to-nation reading''s binding force is contestable and regional. If international law bodies dominate while settler-state courts defer to legislatures, the reading persists in theory but weakens in practice. If Indigenous nation jurisprudence is the primary authority, the reading is strongest within Indigenous legal systems but may lack force in settler-state institutions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_authentication_ambiguity, empirical, 'Which institutional authorities authenticate the nation-to-nation reading and how their verdicts converge or diverge.').

omega_variable(
    extinguishment_foreclosure_ambiguity,
    'Does the nation-to-nation reading logically foreclose the extinguishment reading within a single interpretive framework, or do they coexist as competing readings held by different parties?',
    'Conceptual: examine whether a single framework (e.g., international treaty law) can coherently hold both that historical treaties extinguished Indigenous sovereignty AND that present-day Indigenous nations hold binding consent rights. If not coherent, foreclosure is real; if coherent (e.g., by distinguishing territorial cession from resource consent), coexistence is structurally possible.',
    'If foreclosure is real (logically incompatible), the readings are in zero-sum competition for institutional authority, and one will eventually dominate. If coexistence is possible, both readings can persist as stable alternatives held by different authorities or applied to different territorial domains.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extinguishment_foreclosure_ambiguity, conceptual, 'Whether the nation-to-nation and extinguishment readings are logically incompatible or can coexist within a single framework.').

omega_variable(
    indigenous_consent_internalization,
    'Is suppression in the nation-to-nation reading primarily structural (laws, courts, legislatures actively overriding Indigenous consent) or internalized (Indigenous nations have absorbed the settler-state framing and police their own consent authority downward)?',
    'Empirical: measure the ratio of cases where Indigenous nations reject extraction vs. cases where they accept it despite believing rejection would improve their situation. High acceptance despite stated objections signals internalization. Also compare suppression profiles pre- and post-court rulings that affirm Indigenous consent authority: if internalization drops after court affirmation, suppression was partly internalized.',
    'If suppression is primarily structural (laws overriding consent), the reading''s boundary is at the Indigenous nation seat and enforcement is the settler state''s burden. If suppression is internalized, Indigenous nations carry the suppression internally (learned helplessness, resource dependence, normalized subordination), and the reading''s effective scope is narrower than its formal scope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_consent_internalization, empirical, 'Whether measured suppression is structural enforcement or internalized Indigenous self-limitation.').

omega_variable(
    international_law_binding_force,
    'Does international treaty law (UN frameworks, treaty monitoring committees) carry genuine binding force on settler-state behavior, or is it performative accountability that does not alter resource extraction patterns?',
    'Empirical: correlate international monitoring committee findings of treaty violation with subsequent changes in settler-state behavior (policy shifts, project cancellation, increased consultation). If violations are documented but behavior unchanged, international law is performative. If violations trigger behavioral shifts (even delayed), law carries force.',
    'If international law carries force, suppression_requirement is high but enforcement is distributed (Indigenous nations can invoke international bodies). If international law is performative, suppression falls primarily on Indigenous nations to change behavior through domestic legal and political mobilization, and the reading''s practical scope narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_law_binding_force, empirical, 'Whether international treaty law monitoring produces behavioral compliance or remains symbolic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1975, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1975, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1975, 0.15).
narrative_ontology:measurement(hist_tr_t1990, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(hist_tr_t2005, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2005, 0.32).
narrative_ontology:measurement(hist_tr_t2015, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(hist_tr_t2020, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(hist_tr_t2026, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2026, 0.42).

% Extraction over time
narrative_ontology:measurement(hist_be_t1975, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1975, 0.42).
narrative_ontology:measurement(hist_be_t1990, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1990, 0.48).
narrative_ontology:measurement(hist_be_t2005, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2005, 0.58).
narrative_ontology:measurement(hist_be_t2015, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2015, 0.65).
narrative_ontology:measurement(hist_be_t2020, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(hist_be_t2026, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2026, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1975, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1975, 0.38).
narrative_ontology:measurement(hist_su_t1990, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(hist_su_t2005, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2005, 0.58).
narrative_ontology:measurement(hist_su_t2015, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2015, 0.68).
narrative_ontology:measurement(hist_su_t2020, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(hist_su_t2026, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2026, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(historical_treaty_substrate__nation_to_nation_reading, 0.12).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, indigenous_land_base_governance).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_consent_frameworks).

% DUAL FORMULATION NOTE:
% This constraint is one reading (nation-to-nation) of the contested kernel historical_treaty_substrate. Sibling readings appear as separate constraints: extinguishment_reading treats treaties as completed property sales with no ongoing consent; stewardship_reading treats treaties as relational coexistence pacts without sovereignty cession. The three readings have different ε values, different beneficiary/victim structures, and different institutional substrates. They form a constraint family linked by network.affects_constraints. Each reading instantiates a different constraint classification. The kernel itself (the persisting historical treaty texts and practices) is NOT a constraint; it is the ambiguous substrate the readings disagree about.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(historical_treaty_substrate__nation_to_nation_reading, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
