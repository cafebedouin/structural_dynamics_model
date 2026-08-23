% ============================================================================
% CONSTRAINT STORY: imperial_mandate__loyalist_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imperial_mandate__loyalist_restoration_reading, []).

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
 *   constraint_id: imperial_mandate__loyalist_restoration_reading
 *   human_readable: Loyalist Restoration Reading of the Imperial Mandate (Unmediated Sovereignty Criterion)
 *   domain: political/comparative_constitutional/east_asian_history
 *
 * SUMMARY:
 *   This story instantiates the loyalist_restoration_reading of the
 *   imperial_mandate kernel: the claim that divine mandate requires the
 *   emperor's unmediated exercise of sovereignty, so that legitimacy is
 *   inseparable from active imperial governance and every intermediary
 *   structure (shogunate, samurai class, domain establishments) is usurpation
 *   by definition. The reading crystallized under foreign pressure (1853
 *   onward), drove the Meiji Restoration (1868), and then watched its own
 *   criterion turn on its beneficiaries as oligarchic councils ruled in the
 *   emperor's name. EPSILON REFERENT: the standing arrangement under contest
 *   is the actually-operating imperial-mandate governance order across the
 *   interval - first the bakufu-delegated order, then the Meiji order as
 *   practiced - assessed by THIS reading's criterion. The reading's endorsed
 *   alternative (genuine unmediated rule) is NOT the referent; the high
 *   epsilon records the reading's judgment that the standing order
 *   persistently exercises the mandate without unmediated imperial
 *   governance, first openly, then under sacred cover. CLAIM/METRIC
 *   INDEPENDENCE: claimed_type is authored from structural belief (genuine
 *   coordination function plus asymmetric operation plus active enforcement);
 *   the metrics are authored from descriptive fact; the engine computes
 *   per-seat classifications and any divergence from the claim is the datum.
 *   FAMILY NOTE: the sibling bakufu_delegation_reading shares this referent
 *   and authors low epsilon over it (lawful coordination); the two files are
 *   linked via network.affects_constraints. KEY AGENTS (by structural
 *   relationship): - loyalist_oligarch_coalition: Agenda-setter and principal
 *   beneficiary (institutional/arbitrage) - runs the restored order, captures
 *   its gains - imperial_household: Sacral beneficiary
 *   (powerful/identity_locked) - legitimacy monopoly, agency mediated away -
 *   court_aristocracy_kuge: Secondary beneficiary (moderate/trapped) -
 *   precedence and stipends, total dependence - tokugawa_shogunate: Primary
 *   target (institutional/trapped) - delegitimized into usurper, destroyed -
 *   hereditary_samurai_class: Vanguard turned target
 *   (organized/identity_locked) - made the restoration, was expropriated by
 *   it - han_domain_establishments: Target (organized/constrained) -
 *   abolished wholesale in 1871 - rural_peasantry: Diffuse bearer
 *   (powerless/trapped) - tax and conscription burden, partial emancipation
 *   offset - foreign_treaty_powers: Excluded external pressure
 *   (powerful/mobile) - comparative_constitutional_scholars: Analytical
 *   observer - sees the full kernel architecture
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, 0.8).
domain_priors:suppression_score(imperial_mandate__loyalist_restoration_reading, 0.75).
domain_priors:theater_ratio(imperial_mandate__loyalist_restoration_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(imperial_mandate__loyalist_restoration_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imperial_mandate__loyalist_restoration_reading, tangled_rope).
narrative_ontology:human_readable(imperial_mandate__loyalist_restoration_reading, "Loyalist Restoration Reading of the Imperial Mandate (Unmediated Sovereignty Criterion)").
narrative_ontology:topic_domain(imperial_mandate__loyalist_restoration_reading, "political/comparative_constitutional/east_asian_history").

domain_priors:requires_active_enforcement(imperial_mandate__loyalist_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imperial_mandate__loyalist_restoration_reading, '3d8c541f-7012-409a-8db9-b35cad1ae210').
narrative_ontology:cs_kernel_codification('3d8c541f-7012-409a-8db9-b35cad1ae210', fixed_text).
narrative_ontology:cs_authority_grounding('3d8c541f-7012-409a-8db9-b35cad1ae210', lineage).
narrative_ontology:cs_interpretation_layer_present('3d8c541f-7012-409a-8db9-b35cad1ae210').
narrative_ontology:cs_reading_relation('3d8c541f-7012-409a-8db9-b35cad1ae210', imperial_mandate__bakufu_delegation_reading, forecloses).
narrative_ontology:cs_axiom('3d8c541f-7012-409a-8db9-b35cad1ae210', foundational, legitimacy_inseparable_from_active_governance).
narrative_ontology:cs_axiom_status(legitimacy_inseparable_from_active_governance, holdable).
narrative_ontology:cs_axiom_grounding('3d8c541f-7012-409a-8db9-b35cad1ae210', legitimacy_inseparable_from_active_governance, theological).
narrative_ontology:cs_axiom('3d8c541f-7012-409a-8db9-b35cad1ae210', secondary, intermediary_authority_constitutes_usurpation).
narrative_ontology:cs_axiom_status(intermediary_authority_constitutes_usurpation, holdable).
narrative_ontology:cs_axiom_grounding('3d8c541f-7012-409a-8db9-b35cad1ae210', intermediary_authority_constitutes_usurpation, theological).
narrative_ontology:cs_reference_frame('3d8c541f-7012-409a-8db9-b35cad1ae210', unmediated_imperial_sovereignty).
narrative_ontology:cs_drift_state('3d8c541f-7012-409a-8db9-b35cad1ae210', meiji_oligarchic_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3d8c541f-7012-409a-8db9-b35cad1ae210', '').
narrative_ontology:cs_kernel_id(imperial_mandate__loyalist_restoration_reading, imperial_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, imperial_household).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, court_aristocracy_kuge).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, loyalist_oligarch_coalition).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_class).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, han_domain_establishments).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imperial_mandate__loyalist_restoration_reading, rural_peasantry).
narrative_ontology:constraint_victim(imperial_mandate__loyalist_restoration_reading, rural_peasantry).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, unbroken_imperial_descent_doctrine).
narrative_ontology:constraint_vindicates(imperial_mandate__loyalist_restoration_reading, sonno_joi_nativist_scholarship).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Held ritual supremacy for centuries while actual administration passed to warrior houses. From 1868 the household is relocated to Tokyo, made legally sacrosanct and above politics, and receives the exclusive right to confer legitimacy on the state. Daily governance is prepared by ministers and senior statesmen who present completed decisions for sanction; the emperor's personal participation in deciding is curated by his advisers. Leaving the sacred role is not a meaningful option - the dynasty's identity and the state's legitimacy are fused.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, imperial_household, beneficiary,
    powerful, generational, identity_locked, national).

% Court families long subordinate to warrior governments are elevated above former daimyo in precedence, granted state stipends, and folded into the new peerage. Their income and status now flow entirely from the central state they help sanctify; they have no independent fiscal base and no path back to their pre-restoration marginality.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, court_aristocracy_kuge, beneficiary,
    moderate, generational, trapped, national).

% Mid-ranking samurai from Satsuma, Choshu, Tosa, and Hizen who led the movement to restore direct rule and now staff the new central government. They set policy, command the conscript army, design the tax, education, and land systems, and present every major decision as the emperor's will. Their authority rests on the unmediated-sovereignty claim; their practice concentrates decision-making in their own councils. They can reshape any rule of the arrangement because they run its machinery.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, loyalist_oligarch_coalition, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, loyalist_oligarch_coalition, beneficiary).

% Held delegated administrative authority for two and a half centuries while honoring the court in Kyoto. Under the restoration criterion its entire mode of existence is recast as theft of the mandate. It attempts reform and court-camp coalition, then resists militarily, and is dissolved after defeat in the Boshin War; its institutions are dismantled or repurposed by the victors. Abandoning the delegation framework would have been self-annihilation, so it could not exit except by dying.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, tokugawa_shogunate, payer,
    institutional, biographical, trapped, national).

% Roughly five to six percent of the population, holding hereditary stipends, sword-bearing rights, and tax exemption. Activists from this class supplied the restoration's shock troops under reverence-for-the-emperor banners. After victory their stipends are commuted to interest-bearing bonds that lose about half their real value, their class privileges are abolished, and those who rise in protest - most decisively at Shiroyama in 1877 - are destroyed by the conscript army their own activism helped create. Their identity is fused with stipend, sword, and service; there is no social position outside the class to retreat into.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, hereditary_samurai_class, payer,
    organized, biographical, identity_locked, national).

% Some 260 semi-autonomous domains with their own fiscal, military, and bureaucratic structures. In 1871 they are abolished wholesale: daimyo are pensioned into a new peerage, domain registers and armies are surrendered to the center, and domain schools and administrations are dissolved. Individual lords gain court rank; the establishments themselves cease to exist.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, han_domain_establishments, payer,
    organized, generational, constrained, regional).

% The overwhelming majority of the population. They gain legal emancipation from the caste order - freedom to move, change occupation, and formally own land - and simultaneously bear the new state's costs: a fixed land tax set at roughly three percent of assessed land value, and conscription from 1873 that removes sons for years of service. Village uprisings against the tax and the draft recur through the 1870s. Exit is unavailable; the tax follows the land.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, rural_peasantry, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imperial_mandate__loyalist_restoration_reading, rural_peasantry, beneficiary).

% Western treaty powers whose gunboat diplomacy created the crisis the restoration answers. They demand open ports, extraterritoriality, and tariff limits, and watch the legitimacy contest from entirely outside the indigenous framework - no seat in the mandate conversation, decisive influence over its stakes.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, foreign_treaty_powers, excluded,
    powerful, biographical, mobile, global).

% Study the mandate tradition across Japanese constitutional history - ritsuryo direct rule, regental and cloistered mediation, warrior-house delegation, the Meiji restoration - and can see which structural elements each reading of the mandate presupposes. Take no side in the contest; observe the full architecture.
narrative_ontology:constraint_stakeholder(imperial_mandate__loyalist_restoration_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imperial_mandate__loyalist_restoration_reading, loyalist_oligarch_coalition).
narrative_ontology:fixing_cost_class(imperial_mandate__loyalist_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Reunifies ritual and administrative sovereignty in a single legitimating center, resolving the dual-authority paralysis (court in Kyoto, camp in Edo) that left the country unable to mount a coherent response to foreign pressure; provides the focal point for centralized taxation, conscription, compulsory education, and the coordinated modernization drive.
% TRANSFER_FUNCTION: Moves governing authority and its material supports from hereditary intermediaries (shogunate, domains, samurai stipends) to a central imperial state administered by the restoration coalition; moves land tax revenue and military service from the peasantry into the new central treasury and army.
% ABSENT_VOICES: The emperor's own unmediated will - the voice this criterion exists to center - is structurally absent from actual governance decisions, mediated by the oligarchs who invoke it. The commoners who fund and fill the new army had no seat in the restoration's design. Bakufu-side constitutional thought (lawful coexistence of court and camp) was silenced by military victory rather than answered on its merits.
% DISAPPEARANCE_RATIONALE: If the unmediated-sovereignty criterion vanished overnight, the restored order's legitimacy chain breaks at every joint: the oligarchs govern only as the emperor's direct instruments, the conscript army serves only the sacred sovereign, the land tax is paid only to the imperial state. Either the emperor must actually govern (no machinery, no tradition, no preparation), or legitimacy must be re-founded on delegation (reviving the bakufu logic its bearers just destroyed), or on a wholly new source such as popular sovereignty - the alternative the victors suppressed.
% FOUNDING_PROBLEM: The dual-sovereignty inheritance: ritual authority concentrated in an unworldly court while administrative power was exercised by warrior houses holding delegated mandate - a structure that produced paralysis precisely when foreign powers demanded a unified national response.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: bakufu-side reformers (the kobu-gattai coalition and advisors such as Yokoi Shonan) attested the paralysis while arguing for reformed delegation rather than rupture; contemporary diplomatic records document the dual structure's inability to answer the treaty powers uniformly; postwar historiography (Maruyama Masao and successors) traces how the restored order re-created mediation under imperial cover. The corroboration attests the founding problem was live; it disputes that unmediated rule was the necessary cure.
narrative_ontology:disappearance_verdict(imperial_mandate__loyalist_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(imperial_mandate__loyalist_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imperial_mandate__loyalist_restoration_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(imperial_mandate__loyalist_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imperial_mandate__loyalist_restoration_reading, 0.8, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imperial_mandate__loyalist_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imperial_mandate__loyalist_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imperial_mandate__loyalist_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Why tangled_rope as the structural claim: the arrangement possesses a genuine coordination function - a single legitimating center resolved the dual-authority paralysis and enabled unified fiscal-military response, central taxation, conscription, and the modernization drive; AND it operates asymmetrically - the intermediate orders that structured Japanese life for centuries were annihilated, the activist vanguard that made the restoration was expropriated by it, and the gains concentrated in the restoration coalition; AND it requires active enforcement - civil war (1868-69), the conscript army, and the developing police state. All three tangled_rope conditions are declared structurally. Extractiveness is authored at 0.80: the standing order persistently fails the reading's own criterion, and the failure is not free - it is financed by samurai expropriation, domain abolition, and peasant tax-and-draft burdens. Suppression is authored at 0.75 as a RAW STRUCTURAL PROPERTY - the coercive machinery the arrangement requires - and is deliberately NOT scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater_ratio 0.55 at end-state: after 1868 the unmediated-rule claim is increasingly performed rather than practiced - sanction rituals multiply as actual decision-making consolidates in oligarchic councils; the rising theater series is Goodhart drift of a restoration ideal into court ceremony. Accessibility_collapse 0.72: within the reading's own framework, accepting the inseparability premise collapses every delegation arrangement as illegitimate-by-definition (see the foreclosure edge to the sibling), though in the world alternatives persisted until militarily defeated. Resistance 0.72: Boshin War, domain resistance, the Satsuma Rebellion, and recurring peasant uprisings against tax and draft. All three metric series run on ONE SHARED TIME GRID (t=0,6,12,18,24,30 mapping to 1853,1859,1865,1871,1877,1883) so no metric borrows another's end-state values; the suppression_requirement series is authored because the story specifically tracks enforcement-capacity buildup (civil war mobilization, conscription, police maturation), not merely shifting extraction. Base_properties values equal the series end-states by construction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the agenda-setter seat (loyalist_oligarch_coalition), the arrangement is the fulfillment of a sacred promise they made good at sword-point - genuine coordination they built and now run. From the payer seats, the same structure is annihilation: the shogunate experiences the criterion as the conversion of its 250-year office into a crime; the samurai class experiences it as expropriation by the movement it staffed; the domains experience it as dissolution. The peasantry sits between - emancipation from the caste order against a fixed tax and the draft, with no seat at the table where either was decided. Identity-lock dynamics bind two seats in opposite directions: the samurai class is locked by professional-relational fusion (stipend, sword, and service constituted the self; exit meant ceasing to be a samurai, which is why the class fought at Shiroyama rather than reconstituting), while the imperial household is locked by institutional-sacral fusion (the dynasty's identity and the state's legitimacy became the same object; the emperor cannot decline the role the criterion assigns him). Same-level lateral differentiation: within the samurai class itself, activists who joined the restoration coalition and conservatives who defended the stipend system occupied the same nominal rank and met opposite fates - differentiated not by power but by their structural relationship to the winning coalition. Coalition potential: the payer classes each held latent coalition capacity - the samurai class exercised theirs too late (1877) against a conscript state their own activism had armed; peasant coalitions stayed local and episodic, crushed piecemeal.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. The loyalist_oligarch_coalition declares as agenda_setter and beneficiary with arbitrage exit: it sets the rules, collects the gains, and can reshape any term - derivation places it near the full-beneficiary end (d near 0), and effective extraction inverts toward subsidy. The imperial_household declares as beneficiary with identity_locked exit: it receives the legitimacy monopoly but cannot act on or exit its position - low d, but bound. The court_aristocracy_kuge: beneficiary, trapped - low d with no mobility premium. The payer seats - tokugawa_shogunate (trapped), hereditary_samurai_class (identity_locked), han_domain_establishments (constrained), rural_peasantry (trapped) - derive high d toward the full-target end; trapped and identity-locked targets sit nearer full-target than mobile ones, so the engine amplifies their effective extraction. The peasantry carries a secondary beneficiary declaration (legal emancipation) that partially damps an otherwise maximal target position. Spatial scope is national for nearly all seats: larger scope raises verification difficulty, scaling effective extraction modestly upward engine-side. Foreign_treaty_powers are excluded rather than coordinated - outside the derivation entirely, shaping stakes without holding a seat. The engine owns the chi arithmetic; these declarations are its inputs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (dual-sovereignty paralysis) was live at t0 and plausibly addressed by the restoration - which blocks a pure-snare mislabeling: a reading that saw only extraction would miss the real coordination achievement (unified mobilization, centralized finance, the modernization drive that let Japan escape the treaty-power trap). Conversely, a pure-rope mislabeling would miss the vanguard discard, the theater gap, and the concentration of gains in the restoration coalition. The tangled_rope claim preserves both halves. The lifecycle risk is mandatrophy-in-progress: the founding problem's status is contested (the paralysis was real; whether unmediated rule was the necessary cure is disputed), while the arrangement is maximally load-bearing (disappearance_verdict: world_rearranges) - the mismatch signature to watch is the arrangement persisting on inertia and sanctity after its criterion has been betrayed in practice by its own beneficiaries. The theater_ratio series (0.25 rising to 0.55) is the quantitative trace of that decay: the restoration ideal progressively replaced by performance of the restoration ideal. If the theater gap resolves as full capture (omega unmediated_rule_theater_gap), the recomputed classification for the agenda-setter seat should shift toward the capture end; if the coordination function proves irreplaceable (omega kernel_separability_contest), the extraction ledger rebalances toward transition cost.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_separability_contest,
    'Is the emperor''s legitimacy-granting function separable from the governing function (the bakufu_delegation_reading''s premise), or does legitimacy collapse unless sovereignty is exercised unmediated (this reading''s premise)?',
    'Counterfactual institutional comparison: whether a reformed court-camp condominium along the kobu-gattai line could plausibly have delivered coordinated fiscal-military modernization while retaining delegated mandate, assessed against the actual failure record of bakufu reform attempts 1860-1867.',
    'If separable, this reading''s rupture demand was one solution among several and the destruction of the intermediate orders was contingent rather than necessary; if inseparable, the rupture was the minimum price of coordinated statehood and the cost ledger shifts toward unavoidable transition expense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_separability_contest, conceptual, 'Whether mandate legitimacy and governing function are structurally separable - the axis on which this reading and its sibling diverge.').

omega_variable(
    unmediated_rule_theater_gap,
    'After 1868, was sovereignty actually exercised unmediated by the emperor, or did oligarchic councils decide policy and present finished decisions for imperial sanction?',
    'Documentary reconstruction of decision pathways: genro consultation records, council deliberations, and the actual sequence between proposal and imperial sanction, searched for any recorded instance of imperial initiative overriding ministerial consensus.',
    'If mediation dominated, the reading''s core premise was betrayed by its own beneficiaries and the unmediated-sovereignty criterion operates as legitimation cover for a renewed intermediary elite - pushing computed classifications toward the capture end for the agenda-setter seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unmediated_rule_theater_gap, empirical, 'Gap between the unmediated-rule claim and post-restoration decision practice.').

omega_variable(
    vanguard_discard_extraction,
    'Was the destruction of the samurai class a necessary coordination cost of centralizing the state, or asymmetric expropriation of the class that made the restoration?',
    'Fiscal counterfactual analysis: whether stipend commutation could have preserved option value (gradual conversion, bonds indexed to real value) at acceptable fiscal cost, compared with the imposed bond conversion''s roughly fifty percent real loss.',
    'A necessary-cost finding weights the hybrid structure toward its coordination half; an expropriation finding weights it toward the extraction half and implicates the beneficiary coalition as deliberate capturers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vanguard_discard_extraction, empirical, 'Status of the samurai expropriation within the restoration''s cost structure.').

omega_variable(
    peasant_net_position,
    'Did the peasantry''s net position improve under the restored order (legal emancipation, standardized taxation) or deteriorate (fixed land tax above prior average burden, conscription)?',
    'Regional tax-burden series spanning the 1873 land tax reform, correlated with the geography and frequency of village uprisings 1871-1883.',
    'If net deterioration, the payer set includes the demographic majority and effective extraction on the trapped seats rises sharply; if net improvement, the emancipation offset dampens the payer-seat classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peasant_net_position, empirical, 'Net welfare position of the majority population under the restored order.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imperial_mandate__loyalist_restoration_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t0, imperial_mandate__loyalist_restoration_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t0, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t6, imperial_mandate__loyalist_restoration_reading, theater_ratio, 6, 0.28).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t6, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t12, imperial_mandate__loyalist_restoration_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t12, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t18, imperial_mandate__loyalist_restoration_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t18, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t24, imperial_mandate__loyalist_restoration_reading, theater_ratio, 24, 0.48).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t24, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_tr_t30, imperial_mandate__loyalist_restoration_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(imperial_mandate_loyalist_be_t0, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t0, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t6, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 6, 0.76).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t6, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t12, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 12, 0.86).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t12, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t18, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 18, 0.72).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t18, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t24, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 24, 0.77).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t24, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_be_t30, imperial_mandate__loyalist_restoration_reading, base_extractiveness, 30, 0.8).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(imperial_mandate_loyalist_su_t0, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t0, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t6, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t6, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t12, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 12, 0.78).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t12, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t18, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 18, 0.72).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t18, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t24, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t24, observed).
narrative_ontology:measurement(imperial_mandate_loyalist_su_t30, imperial_mandate__loyalist_restoration_reading, suppression_requirement, 30, 0.75).
narrative_ontology:measurement_basis(imperial_mandate_loyalist_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imperial_mandate__loyalist_restoration_reading, identity_coordination).
narrative_ontology:affects_constraint(imperial_mandate__loyalist_restoration_reading, bakufu_delegation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'imperial mandate' conflates two structurally distinct claims: (1) the mandate OPERATES THROUGH delegation - the legitimacy-granting function is separable from the governing function (bakufu_delegation_reading); (2) the mandate REQUIRES unmediated exercise - legitimacy is void unless the emperor governs directly (this file). The two readings share the referent (the standing imperial-mandate governance order) and author widely different epsilon over it because they apply incompatible criteria; they are modeled as two linked stories, not one story with a measurement parameter. Downstream descendants of this reading (State Shinto kokutai doctrine, the 1889 Constitution's imperial-sovereignty clause) inherit its inseparability premise and are candidates for separate stories in this family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
