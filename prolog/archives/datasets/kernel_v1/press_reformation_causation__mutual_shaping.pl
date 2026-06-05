% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__mutual_shaping, []).

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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Press-Reformation Mutual Shaping Constraint
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   The mutual-shaping reading instantiates bidirectional causation between
 *   printing press technology and Reformation religious movements. Neither
 *   the press nor the Reformation determined the other; instead, they
 *   co-evolved through recursive feedback loops. Early printing technology
 *   (mechanical reproduction, standardized text, decentralized production)
 *   created new possibilities for distributing ideas at scale. Reformers
 *   recognized and exploited these possibilities — translating scripture into
 *   vernacular, printing polemical pamphlets, organizing networks of
 *   distributed printers. This intensive use by reformers, in turn, shaped
 *   how printing technology developed: printers adapted production methods to
 *   handle high-volume religious texts, invested in vernacular typefaces,
 *   developed distribution networks that followed theological lines rather
 *   than commercial ones. The constraint exhibits both coordination (the
 *   press enables communication infrastructure that multiple parties require)
 *   and asymmetric extraction (access to printing capacity and market reach
 *   concentrate power among certain reformer factions and large
 *   merchant-printers, while others lack this access). The mutual-shaping
 *   reading contrasts with two sibling readings: technological_determinism
 *   (which treats the printing press as an independent cause that made the
 *   Reformation inevitable) and strategic_deployment (which treats reformers
 *   as strategically using a neutral technology as a means to predefined
 *   ends, with technology as passive tool). This reading insists the
 *   relationship was more intimate: technology provided structure and
 *   affordances; reformers' choices modified which affordances became
 *   salient; the technology's subsequent development reflected those choices
 *   back into the system.
 *
 * KEY AGENTS:
 *   - Reformers (Martin Luther, Zwingli, Calvin, etc.): Primary beneficiary + strategic actor (organized/constrained) — exploit printing capacity, but depend on printers; their theological innovations are amplified by distribution, but their authority is diluted by competing printed texts
 *   - Printers/Merchant-Guilds (Gutenberg, the Estienne family, Basel printers): Beneficiary and infrastructure provider (powerful/arbitrage) — profit from heterodox religious market; control production bottleneck; can shift between orthodox and heterodox markets
 *   - Catholic Ecclesiastical Authority: Victim and suppressor (institutional/constrained) — loses monopoly on scriptural interpretation and text production; enforcement costs (book banning, suppression) rise as volume scales; authority erodes through inability to control distributed texts
 *   - Manuscript Scribal Economy: Victim (powerless/trapped) — economically displaced by mechanical reproduction; no viable exit from disrupted production mode; epistemic authority over texts is forfeit
 *   - Reformation Movement Network: Organized beneficiary + constrained actor (organized/constrained) — coordinates around printing distribution; experiences both coordination function (shared infrastructure) and internal asymmetric extraction (access concentration)
 *   - Analytical Observer: Witnesses mutual shaping (analytical/analytical) — risks misclassifying mutual shaping as either pure technological determinism (natural law/mountain) or pure strategic choice (intentional tools/rope)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.35).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.42).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, tangled_rope).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Press-Reformation Mutual Shaping Constraint").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'd55528d8-3aed-411f-be1c-7d2524a4f0ae').
narrative_ontology:cs_kernel_codification('d55528d8-3aed-411f-be1c-7d2524a4f0ae', distributed).
narrative_ontology:cs_authority_grounding('d55528d8-3aed-411f-be1c-7d2524a4f0ae', distributed).
narrative_ontology:cs_reading_relation('d55528d8-3aed-411f-be1c-7d2524a4f0ae', press_reformation_causation__technological_determinism, influences).
narrative_ontology:cs_reading_relation('d55528d8-3aed-411f-be1c-7d2524a4f0ae', press_reformation_causation__strategic_deployment, influences).
narrative_ontology:cs_axiom('d55528d8-3aed-411f-be1c-7d2524a4f0ae', foundational, bidirectional_causation_between_technology_and_agency).
narrative_ontology:cs_axiom_status(bidirectional_causation_between_technology_and_agency, holdable).
narrative_ontology:cs_axiom_grounding('d55528d8-3aed-411f-be1c-7d2524a4f0ae', bidirectional_causation_between_technology_and_agency, empirically_contingent).
narrative_ontology:cs_axiom('d55528d8-3aed-411f-be1c-7d2524a4f0ae', foundational, reformer_choice_shaped_technological_development).
narrative_ontology:cs_axiom_status(reformer_choice_shaped_technological_development, holdable).
narrative_ontology:cs_axiom_grounding('d55528d8-3aed-411f-be1c-7d2524a4f0ae', reformer_choice_shaped_technological_development, empirically_contingent).
narrative_ontology:cs_reference_frame('d55528d8-3aed-411f-be1c-7d2524a4f0ae', equilibrium_before_scaling).
narrative_ontology:cs_drift_state('d55528d8-3aed-411f-be1c-7d2524a4f0ae', mid_sixteenth_century_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d55528d8-3aed-411f-be1c-7d2524a4f0ae', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, printers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_authority).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_scribal_economy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MANUSCRIPT SCRIBAL ECONOMY (SNARE) — Trapped in a production mode rendered obsolete by competing technology. No exit from mechanical reproduction without abandoning craft entirely. Experiences the constraint as pure extraction of their economic role and epistemic authority. Cannot organize effective resistance; cannot control how texts are reproduced once printing becomes viable.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: CATHOLIC ECCLESIASTICAL AUTHORITY (SNARE) — Constrained by rapidly proliferating vernacular texts beyond institutional control mechanisms. Suppression (book banning, burning) remains technically possible but becomes increasingly expensive as volume scales. Extraction flows away from this agent — control over scriptural interpretation and text production is forfeit. Authority erodes through sheer volume of alternative texts. High suppression requirement (enforcement costs rising) but effective extraction (χ) remains high because the constraint removes institutional monopolies.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: INDIVIDUAL REFORMER (TANGLED ROPE) — Benefits from distributed printing capacity (could not have achieved theological reach without it), but also bears extraction in the form of immediate legal/physical danger and dependence on printer networks. Coordination function: reformers enable printers by creating demand and legitimacy for heterodox texts. Asymmetric extraction: printer captures economic benefit while reformer bears heavier personal risk. Neither pure extraction nor pure coordination.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRINTER/MERCHANT-GUILD (ROPE) — Benefits from profitable market for religious texts, both orthodox and heterodox. Can arbitrage between markets (print official liturgies for revenue, Protestant texts for growth). Coordination function primary: enables communication infrastructure that multiple parties (reformers, humanists, scholars) require. Extraction is secondary and negotiable. Exit option (arbitrage between markets) is real; can shift focus if one market becomes too risky.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: REFORMATION MOVEMENT/NETWORK (TANGLED ROPE) — Organized coordination around theological claims and textual distribution. Coordination function: press enables rapid distribution of ideas and creates feedback loops (local printing, local dissemination, network effects). Asymmetric extraction: printing infrastructure amplifies some voices (those with access to printers, funding) and suppresses others (those without network connections). Internal hierarchy emerges within the movement itself. Constrained because the movement's success depends on maintaining printer relationships — cannot unilaterally control the technology.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: TECHNOLOGICAL DETERMINISM (MOUNTAIN CANDIDATE) — From a civilizational view, the printing press caused the Reformation by making censorship impossible and vernacular scripture inevitable. This perspective treats the constraint as natural law: technology determined outcomes; agency had no real choice. However, the structural data reveals beneficiaries (reformers, printers) whose active choices shaped which technologies scaled and how. The mutual-shaping reading forecloses this as a false summit — the appearance of inevitability masks contingent institutional decisions.
constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(press_reformation_causation__mutual_shaping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint exhibits genuine coordination (press enables distribution infrastructure, reformers require it, printers profit from serving both markets) alongside asymmetric extraction (access to printing capacity concentrates power; large organized groups benefit more than dispersed individuals; scribal economy is displaced). The value reflects that extraction is real but not overwhelming — the press also enables decentralized printing, which partially counteracts centralization. Suppression (0.42): Moderate. Catholic authorities suppress heterodox texts through burning, banning, legal penalties — mechanisms that remain technically available but become increasingly expensive as volume scales. The suppression is substantial and active (not low), but not so comprehensive as to foreclose all heterodox publishing (snare-level would be 0.60+). Theater ratio (0.48): Moderate. The mutual-shaping reading does not rely on performative framing — the technological and rhetorical changes are substantive. Theater arises primarily in the mythologization of the press as inevitable cause and reformers as passive recipients of its possibilities. The low theater (below 0.5) reflects that real material changes (text standardization, decentralized production, rapid distribution) are happening; the constraint is not sustained by theater but by structure.
 *
 * PERSPECTIVAL GAP:
 *   The mutual-shaping reading generates a perspectival gap between technological determinism and strategic deployment. The technological determinism perspective (mountain) treats the press as an independent causal force that made the Reformation inevitable — censorship became impossible, vernacular scripture became unavoidable, therefore theology changed necessarily. The strategic deployment perspective would treat the press as a neutral tool that reformers deliberately exploited for predefined theological ends — the tool was instrumental, the goal was pre-set, strategy was conscious choice. The mutual-shaping reading occupies the middle ground: neither the press nor the reformers had fully pre-determined intentions or outcomes; instead, each responding to the other in real time, shaping what became possible and salient. This produces tangled_rope classifications from reformer and movement perspectives (genuine coordination + extraction), rope from printer perspectives (arbitrage + coordination), and snare from victim perspectives (Catholic authority, scribal economy). The mountain perspective (technological determinism) is unmasked as a false summit — the appearance of inevitability covers contingent institutional choices about which technologies to invest in, which markets to serve, and which affordances to cultivate.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary reformers (organized/constrained exit) derive d ~0.40-0.55 (victim of their dependence on printers, beneficiary of distribution amplification). Beneficiary printers (powerful/arbitrage exit) derive d ~0.15-0.25 (capture profit, can exit into orthodox market). Victim Catholic authority (institutional/constrained exit) derives d ~0.60-0.75 (suppression is rising cost; extraction of their monopoly is real). Victim scribal economy (powerless/trapped exit) derives d ~0.95 (maximum target status; cannot exit or adapt). The engine applies the sigmoid f(d) to each, producing chi values that scale with these directional positions. Reformers and printers experience moderate chi (mixed coordination/extraction); Catholic authority and scribal economy experience high chi (net extraction flow away from them). The mutual-shaping reading does NOT compute d from pre-determined agent slots; instead, d is derived from the agents' real structural positions relative to THIS constraint — their power, exit options, and beneficiary/victim status within the press-Reformation system specifically.
 *
 * MANDATROPHY ANALYSIS:
 *   The mutual-shaping reading resolves mandatrophy by declining to reduce the constraint to pure technological determinism (mountain) or pure tool use (rope). The core claim is that the relationship between technology and agency is genuinely bidirectional — neither determines the other independently. This produces tangled_rope as the primary classification from multiple perspectives (reformer, movement, printer network contexts). The mountain classification (technological determinism) is exposed as a false summit through the beneficiary/victim analysis: identifiable parties (reformers, printers) made choices that shaped technological development; identifiable victims (Catholic authority, scribal economy) bore costs of those choices. If the constraint were a natural law (mountain), there would be no beneficiaries — natural laws do not benefit agents; they constrain all equally. The presence of asymmetric beneficiary/victim relationships proves the mountain is false. The strategic deployment reading (rope) is partially correct — reformers DID deploy strategically — but incomplete because reformer strategy was itself shaped by what the technology made feasible, which was in turn shaped by printer investments, which were shaped by market demand from reformers and others. Strategic choice existed at every level, but not independently of the technological structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mutual_shaping_counterfactual,
    'Would printing press technology have developed the same features (vernacular emphasis, decentralized production, rapid iteration) WITHOUT Reformation demand, or did reformer needs actively shape what printing became?',
    'Historical analysis of pre-Reformation printing (liturgical, classical texts, technical manuals) vs post-Reformation printing; comparison of printing development in Protestant vs Catholic regions; examination of printer investment decisions and market signals',
    'If printing developed identically regardless of religious demand: mutual shaping is retrospective narrative (constraint is closer to technological_determinism). If printing adapted features specifically for religious/reformer markets: mutual shaping is structural (this reading holds; constraint is authentic tangled_rope, not mountain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mutual_shaping_counterfactual, empirical, 'Whether reformer demand actively shaped printing technology development').

omega_variable(
    reformer_agency_vs_opportunity,
    'Did reformers exercise strategic choice in exploiting printing (strategic_deployment reading), or did their use of printing represent constrained choices within a tech-enabled opportunity structure (mutual_shaping reading)?',
    'Documentary evidence of reformer deliberation about printing strategy; comparison with non-religious movements'' use of printing; analysis of whether reformers could have organized effectively WITHOUT printing',
    'If deliberate strategy: strategic_deployment reading prevails; constraint is rope (intentional coordination use). If constrained exploitation of available structure: mutual_shaping prevails; constraint is tangled_rope (both coordinating and extracting). If either could be true from different sources: readings coexist_with each other.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformer_agency_vs_opportunity, conceptual, 'Whether reformer use of printing was strategic choice or constrained exploitation').

omega_variable(
    reading_equivalence_under_frame_shift,
    'From the perspective of economic history (printer profit, market development), does mutual_shaping reduce to strategic_deployment? From ideological history (theological innovation), does it reduce to technological_determinism?',
    'Frame-dependent analysis: are the three readings genuinely distinct structural claims, or do they converge under different analytical frames?',
    'If readings are frame-equivalent: the constraint instantiates the oracle gap (Theorem 4) — the kernel contest is a notational variant rather than a substantive disagreement. If readings are truly distinct: they represent genuine alternative commitments about causation, agency, and structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_equivalence_under_frame_shift, conceptual, 'Whether the three readings are genuinely distinct or frame-equivalent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_ref_theater_t0, press_reformation_causation__mutual_shaping, theater_ratio, 0, 0.35).
narrative_ontology:measurement(press_ref_theater_t25, press_reformation_causation__mutual_shaping, theater_ratio, 25, 0.42).
narrative_ontology:measurement(press_ref_theater_t50, press_reformation_causation__mutual_shaping, theater_ratio, 50, 0.48).

% Extraction over time
narrative_ontology:measurement(press_ref_extractiveness_t0, press_reformation_causation__mutual_shaping, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(press_ref_extractiveness_t25, press_reformation_causation__mutual_shaping, base_extractiveness, 25, 0.3).
narrative_ontology:measurement(press_ref_extractiveness_t50, press_reformation_causation__mutual_shaping, base_extractiveness, 50, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(press_ref_suppression_t0, press_reformation_causation__mutual_shaping, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(press_ref_suppression_t25, press_reformation_causation__mutual_shaping, suppression_requirement, 25, 0.38).
narrative_ontology:measurement(press_ref_suppression_t50, press_reformation_causation__mutual_shaping, suppression_requirement, 50, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, reformation_theological_innovation).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, manuscript_to_print_transition_economic).

% DUAL FORMULATION NOTE:
% The press-reformation constraint operates at the meta-level of how technology and social movements interact. Downstream constraints (reformation_theological_innovation, manuscript_to_print_transition_economic) decompose specific empirical claims: whether theological claims were genuinely novel (or merely newly distributed) and whether the manuscript economy was displaced by economics (or by technology + economics). The mutual_shaping reading is agnostic on these downstream specifics; it claims only that bidirectional causation operated at the technology-movement interface.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
