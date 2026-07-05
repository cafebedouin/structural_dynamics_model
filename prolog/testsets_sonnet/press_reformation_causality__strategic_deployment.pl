% ============================================================================
% CONSTRAINT STORY: press_reformation_causality__strategic_deployment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causality__strategic_deployment, []).

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
 *   constraint_id: press_reformation_causality__strategic_deployment
 *   human_readable: Strategic Weaponization of the Printing Press Against Ecclesiastical Authority
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   This story instantiates the strategic_deployment reading of the
 *   press_reformation_causality kernel: reformers and printers are read here
 *   as intentional agents who selected, financed, and timed print output to
 *   achieve religious displacement and economic gain, using the press as an
 *   instrument in an active contest against Roman ecclesiastical authority.
 *   The technology itself functions as coordination infrastructure (rope-like
 *   — genuinely solving the distribution/capital problem for reform networks)
 *   while the deployment against the Church's monopoly constitutes an
 *   extractive campaign with named victims (curia revenue, clerical
 *   authority, loyalist populations, scriptoria labor), which is why the
 *   constraint as a whole reads as tangled_rope rather than pure rope or pure
 *   snare: the coordination function is real and the asymmetric extraction
 *   from Church authority and bystander populations is also real, and both
 *   ride the same printing infrastructure. This is one of three sibling
 *   readings of the same underlying kernel; technological_determinism reads
 *   the press as an autonomous mountain-like force making Reformation success
 *   inevitable regardless of any actor's intent, and co_constitution reads
 *   causality as an irreducible feedback loop between technology and agency
 *   that resists assignment of primary agency to either side. The
 *   strategic_deployment reading differs from both by insisting on
 *   identifiable, intentional actors making strategic choices — reformers
 *   picking print runs, printers picking patrons, princes picking allegiances
 *   — which is what licenses beneficiary/victim declarations and the
 *   tangled_rope classification here; the sibling readings would not license
 *   the same declarations.
 *
 * KEY AGENTS:
 *   - protestant_reformers: primary agenda-setter (organized/mobile) — commission and time propaganda strategically
 *   - printer_guilds: beneficiary/co-agenda-setter (organized/arbitrage) — profit from reform print demand, relocate across jurisdictions
 *   - roman_curia: primary target (institutional/constrained) — loses doctrinal monopoly and revenue
 *   - catholic_loyalist_populations: diffuse payer (powerless/trapped) — bears confessional conflict costs with no strategic input
 *   - historians_of_the_reformation: analytical observer — reconstructs intentionality from correspondence and financial records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, 0.62).
domain_priors:suppression_score(press_reformation_causality__strategic_deployment, 0.58).
domain_priors:theater_ratio(press_reformation_causality__strategic_deployment, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, extractiveness, 0.62).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(press_reformation_causality__strategic_deployment, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causality__strategic_deployment, tangled_rope).
narrative_ontology:human_readable(press_reformation_causality__strategic_deployment, "Strategic Weaponization of the Printing Press Against Ecclesiastical Authority").
narrative_ontology:topic_domain(press_reformation_causality__strategic_deployment, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causality__strategic_deployment).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causality__strategic_deployment, '613cf266-df4d-49cc-b466-5342aa336b0c').
narrative_ontology:cs_kernel_codification('613cf266-df4d-49cc-b466-5342aa336b0c', distributed).
narrative_ontology:cs_authority_grounding('613cf266-df4d-49cc-b466-5342aa336b0c', distributed).
narrative_ontology:cs_reading_relation('613cf266-df4d-49cc-b466-5342aa336b0c', press_reformation_causality__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('613cf266-df4d-49cc-b466-5342aa336b0c', press_reformation_causality__co_constitution, influences).
narrative_ontology:cs_axiom('613cf266-df4d-49cc-b466-5342aa336b0c', foundational, print_deployment_was_intentional_strategic_choice).
narrative_ontology:cs_axiom_status(print_deployment_was_intentional_strategic_choice, holdable).
narrative_ontology:cs_axiom_grounding('613cf266-df4d-49cc-b466-5342aa336b0c', print_deployment_was_intentional_strategic_choice, empirically_contingent).
narrative_ontology:cs_axiom('613cf266-df4d-49cc-b466-5342aa336b0c', secondary, economic_and_doctrinal_motives_are_jointly_sufficient_explanation).
narrative_ontology:cs_axiom_status(economic_and_doctrinal_motives_are_jointly_sufficient_explanation, holdable).
narrative_ontology:cs_axiom_grounding('613cf266-df4d-49cc-b466-5342aa336b0c', economic_and_doctrinal_motives_are_jointly_sufficient_explanation, empirically_contingent).
narrative_ontology:cs_reference_frame('613cf266-df4d-49cc-b466-5342aa336b0c', clerical_interpretive_monopoly).
narrative_ontology:cs_drift_state('613cf266-df4d-49cc-b466-5342aa336b0c', post_diet_of_worms_print_saturation, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('613cf266-df4d-49cc-b466-5342aa336b0c', '').
narrative_ontology:cs_kernel_id(press_reformation_causality__strategic_deployment, press_reformation_causality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, printer_guilds).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, territorial_princes).
narrative_ontology:constraint_beneficiary(press_reformation_causality__strategic_deployment, vernacular_pamphlet_publishers).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, roman_curia).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, latin_literate_clergy).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, catholic_loyalist_populations).
narrative_ontology:constraint_victim(press_reformation_causality__strategic_deployment, traditional_manuscript_scriptoria).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, print_as_instrument_of_partisan_persuasion).
narrative_ontology:constraint_vindicates(press_reformation_causality__strategic_deployment, media_control_as_precondition_for_doctrinal_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther, his allies, and successor reform networks deliberately commission pamphlets, broadsheets, woodcuts, and vernacular Bible translations, coordinating with printers to flood specific markets ahead of Church response cycles. They select print runs, timing, and imagery to maximize doctrinal reach and to outpace ecclesiastical censorship, treating the press as a weapon in an active contest for religious authority.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, protestant_reformers, agenda_setter,
    organized, generational, mobile, continental).

% Printing workshops in Wittenberg, Strasbourg, Basel, and Antwerp profit directly from reform pamphlet demand, often prioritizing cheap, fast, high-volume reformist tracts over slower, less lucrative Latin theological works. They relocate operations across jurisdictional lines to evade licensing controls, arbitraging between competing political and confessional patrons for commissions.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, printer_guilds, beneficiary,
    organized, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causality__strategic_deployment, printer_guilds, agenda_setter).

% German princes and city councils sympathetic to reform use printed propaganda to legitimate confiscation of Church lands and assertion of territorial religious authority, subsidizing presses and censoring in the opposite direction once in power. Their exit option is realignment of political-religious allegiance whenever it serves consolidation of local sovereignty.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, territorial_princes, beneficiary,
    institutional, generational, mobile, national).

% Independent pamphleteers and translators profit from commissioning and distributing vernacular scripture and polemical literature, building careers and reputations on the controversy itself. They can shift output toward whichever confessional market pays best.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, vernacular_pamphlet_publishers, beneficiary,
    moderate, biographical, mobile, regional).

% The papal curia and allied bishoprics lose doctrinal monopoly and revenue (indulgence sales, tithes, patronage networks) as printed reform literature outpaces their capacity to respond through traditional manuscript and pulpit channels. Their exit options are constrained to counter-printing, index censorship, and political suppression, all reactive and structurally slower than the reform print networks.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, roman_curia, payer,
    institutional, civilizational, constrained, continental).

% Parish priests and lower clergy whose authority rested on exclusive access to Latin scripture and interpretive mediation find their interpretive monopoly bypassed by vernacular print. Many are trapped in dioceses experiencing rapid confessional realignment with little personal capacity to relocate or retrain.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, latin_literate_clergy, payer,
    moderate, biographical, trapped, regional).

% Ordinary parishioners in contested territories experience confessional conflict, iconoclasm, and sometimes violence as competing pamphlet campaigns polarize communities. They have essentially no control over which confessional print campaign saturates their local market or which authority ultimately prevails locally.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, catholic_loyalist_populations, payer,
    powerless, biographical, trapped, local).

% Monastic and cathedral scriptoria producing hand-copied liturgical and theological texts lose commissions and prestige as print output overwhelms manuscript production economically and in volume, with no realistic transition path for their specialized labor.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, traditional_manuscript_scriptoria, payer,
    powerless, biographical, trapped, local).

% Later scholars examine surviving print runs, correspondence between reformers and printers, and financing records to reconstruct the degree to which print deployment was deliberate strategy versus emergent technological diffusion.
narrative_ontology:constraint_stakeholder(press_reformation_causality__strategic_deployment, historians_of_the_reformation, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causality__strategic_deployment, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causality__strategic_deployment, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reformers and printers coordinate scarce capital, technical skill, and distribution networks to produce and circulate vernacular religious material at a scale and speed no single actor could achieve alone — a genuine solution to the collective-action problem of challenging an entrenched, geographically dispersed ecclesiastical monopoly.
% TRANSFER_FUNCTION: The arrangement transfers doctrinal authority, tithe and indulgence revenue, and interpretive control away from the Roman Curia and Latin-literate clergy toward reform leadership, printer-publishers, and territorial princes, while imposing confessional conflict costs on ordinary parishioners and displacing manuscript labor.
% ABSENT_VOICES: Traditional scriptoria workers and lower clergy losing their functional role rarely appear in the surviving correspondence between reformers and printers, who wrote primarily to each other and to patrons; the ordinary parishioners bearing the costs of confessional conflict left almost no documentary voice in the strategic planning record at all.
% DISAPPEARANCE_RATIONALE: If the deliberate reformer-printer alliance had not existed — if print technology had diffused without organized strategic direction toward doctrinal ends — the pace, geographic pattern, and success rate of confessional realignment would have been substantially different: fewer coordinated pamphlet campaigns, slower vernacular Bible penetration, and likely a different balance of power between reform movements and territorial authorities.
% FOUNDING_PROBLEM: Reform leaders needed a way to reach lay audiences directly and rapidly, bypassing clerical gatekeeping of scripture and doctrine, before ecclesiastical or political authorities could suppress the movement through traditional channels of control.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary Catholic polemicists and later institutional historians of the Counter-Reformation (writing from outside the reform movement) corroborate that the deliberate print campaigns achieved their doctrinal-displacement purpose within a generation; once confessional territories stabilized under cuius regio, eius religio arrangements, the original urgency of bypassing clerical gatekeeping was resolved, though the printer-guild and territorial-prince economic benefits from continued religious pamphleteering persisted well beyond the original founding problem's resolution.
narrative_ontology:disappearance_verdict(press_reformation_causality__strategic_deployment, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causality__strategic_deployment, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causality__strategic_deployment, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causality__strategic_deployment, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causality__strategic_deployment, 0.62, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causality__strategic_deployment_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(press_reformation_causality__strategic_deployment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(press_reformation_causality__strategic_deployment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is set at a moderate-high 0.62 because the campaign transfers real institutional resources (indulgence revenue, land, interpretive authority) away from the Church toward reform leadership and allied princes, but the transfer rides on genuine coordination value (solving reform's distribution problem), so it does not reach snare-level extraction. Suppression at 0.58 reflects active measures on both sides — reform networks suppressing manuscript/Latin channels' relative advantage by flooding markets, and princes suppressing rival confessional print once territorially dominant — rather than one-directional coercion. Theater ratio is low-moderate (0.28) because most of the print activity was functionally aimed at doctrinal persuasion and revenue capture rather than pure performance, though some pamphleteering (woodcut caricature, sensationalist broadsheets) drifted toward theatrical provocation over the interval. Accessibility collapse (0.45) is moderate: alternatives to the printed vernacular channel (oral preaching, manuscript circulation, personal catechesis) persisted throughout the period and were not fully foreclosed, distinguishing this from a mountain-like inevitability claim. Resistance is high (0.7), consistent with the historical record of active Catholic counter-printing, indices of prohibited books, and princely suppression campaigns.
 *
 * DIRECTIONALITY LOGIC:
 *   Protestant reformers, printer guilds, sympathetic princes, and vernacular publishers are declared beneficiaries because the correspondence record shows deliberate commissioning, timing, and capital allocation aimed at doctrinal and economic gain — this yields low directionality (d near beneficiary end) once the engine derives it. The Roman Curia, Latin-literate clergy, loyalist populations, and manuscript scriptoria are declared victims because the same campaigns are aimed at displacing their revenue, authority, and labor respectively, without their strategic consent — this yields high derived directionality (d near target end), amplified for the powerless, trapped populations who had essentially no capacity to exit the confessional conflict imposed on their localities.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (reaching lay audiences before ecclesiastical suppression could occur) is marked dead: once confessional territories stabilized, the original urgency dissolved, yet printer guilds and territorial princes continued profiting from ongoing religious pamphleteering well past that point — a classic mandatrophy signature where the coordination rationale outlives its necessity while the extraction infrastructure persists. Classifying this as tangled_rope rather than snare prevents mislabeling the genuine, time-bound coordination achievement (reaching lay audiences with vernacular scripture) as pure extraction, while the beneficiary/victim/enforcement declarations prevent the opposite error of treating the campaign as costless technological diffusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intentionality_vs_emergent_diffusion,
    'Is the historical record of reformer-printer coordination best read as deliberate, centrally-strategized campaigns, or as emergent, decentralized adoption patterns that only retrospectively look coordinated?',
    'Close archival analysis of correspondence, financing records, and print-run timing between named reformers and specific print shops, cross-referenced against counterfactual diffusion models of print technology absent religious controversy.',
    'If the record supports genuine centralized strategy, the tangled_rope/beneficiary framing of this reading holds; if the pattern is better explained by decentralized market response to demand (co_constitution reading) or autonomous technological diffusion (technological_determinism reading), this reading''s beneficiary declarations overstate the intentionality actually present and the sibling readings should be weighted more heavily in any cross-story synthesis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intentionality_vs_emergent_diffusion, empirical, 'Whether deliberate strategic agency or emergent diffusion best explains the print-Reformation correlation.').

omega_variable(
    kernel_framing_choice_signal,
    'What in the source material licenses selecting the strategic_deployment framing over the technological_determinism or co_constitution framings for THIS story?',
    'The declared expected structural delta for this story explicitly specifies ''reformers/printers as beneficiaries; press as snare deployed against Church authority'' — this framing was assigned by the generation manifest rather than derived independently from primary sources within this story.',
    'Because framing was manifest-assigned rather than independently derived, the classification here should be read as one coherent, internally-consistent reading among three co-equal candidates, not as the historically ''correct'' account; a reader encountering only this file without the sibling files would risk mistaking one reading for the settled historiographical consensus.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_choice_signal, conceptual, 'Documents that this story''s framing choice was assigned by the kernel manifest, not independently derived, per the committer-frame authoring discipline.').

omega_variable(
    beneficiary_boundary_within_reform_movement,
    'Within the broad category of ''protestant_reformers,'' did all factions equally benefit economically, or did some reform leaders (e.g., those without printer patronage networks) bear costs similar to the Church-aligned victims?',
    'Comparative study of financial outcomes across reform leaders with strong versus weak printer relationships (e.g., well-connected Wittenberg circle versus more marginal radical reform factions).',
    'If significant intra-reform variance exists, the beneficiary group should be split into finer-grained subgroups in a more granular follow-up story, rather than treating all reformers as a uniform beneficiary class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_boundary_within_reform_movement, empirical, 'Whether the reformer beneficiary group masks internal variance in who actually profited from print alliances.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causality__strategic_deployment, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t0, press_reformation_causality__strategic_deployment, theater_ratio, 0, 0.1).
narrative_ontology:measurement(pres_tr_t10, press_reformation_causality__strategic_deployment, theater_ratio, 10, 0.15).
narrative_ontology:measurement(pres_tr_t20, press_reformation_causality__strategic_deployment, theater_ratio, 20, 0.2).
narrative_ontology:measurement(pres_tr_t30, press_reformation_causality__strategic_deployment, theater_ratio, 30, 0.24).
narrative_ontology:measurement(pres_tr_t40, press_reformation_causality__strategic_deployment, theater_ratio, 40, 0.26).
narrative_ontology:measurement(pres_tr_t50, press_reformation_causality__strategic_deployment, theater_ratio, 50, 0.27).
narrative_ontology:measurement(pres_tr_t60, press_reformation_causality__strategic_deployment, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(pres_be_t0, press_reformation_causality__strategic_deployment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pres_be_t10, press_reformation_causality__strategic_deployment, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(pres_be_t20, press_reformation_causality__strategic_deployment, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(pres_be_t30, press_reformation_causality__strategic_deployment, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(pres_be_t40, press_reformation_causality__strategic_deployment, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(pres_be_t50, press_reformation_causality__strategic_deployment, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(pres_be_t60, press_reformation_causality__strategic_deployment, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t0, press_reformation_causality__strategic_deployment, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(pres_su_t10, press_reformation_causality__strategic_deployment, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(pres_su_t20, press_reformation_causality__strategic_deployment, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(pres_su_t30, press_reformation_causality__strategic_deployment, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(pres_su_t40, press_reformation_causality__strategic_deployment, suppression_requirement, 40, 0.57).
narrative_ontology:measurement(pres_su_t50, press_reformation_causality__strategic_deployment, suppression_requirement, 50, 0.58).
narrative_ontology:measurement(pres_su_t60, press_reformation_causality__strategic_deployment, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causality__strategic_deployment, identity_coordination).
narrative_ontology:boltzmann_floor_override(press_reformation_causality__strategic_deployment, 0.1).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causality__strategic_deployment, press_reformation_causality__co_constitution).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the press_reformation_causality kernel, linked bidirectionally: strategic_deployment (this file, tangled_rope), technological_determinism (mountain-leaning, press as autonomous inevitability-producing force), and co_constitution (feedback-loop framing resisting singular agency attribution). Each carries its own ε, beneficiary/victim structure, and classification per the ε-invariance principle; none averages or supersedes the others. A reader synthesizing across the family should treat divergent classifications as evidence of genuine unresolved historiographical contest, not as an error requiring reconciliation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
