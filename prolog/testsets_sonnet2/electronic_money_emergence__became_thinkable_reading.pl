% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-17
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__became_thinkable_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: electronic_money_emergence__became_thinkable_reading
 *   human_readable: Digital Money's Conceptual-Diffusion Emergence (Became-Thinkable Reading)
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the 'became thinkable' reading of the electronic
 *   money emergence kernel: digital/electronic money is held to have emerged
 *   when dematerialized value transfer became conceptually and socially
 *   available as a possibility, evidenced by engineering practice
 *   (telegraphic transfer, clearinghouse automation, magnetic-stripe
 *   processing) and theoretical writing on money-as-ledger-claim, well before
 *   any central bank statistical apparatus (M4/M5-style aggregates) formally
 *   distinguished electronic balances as a countable category. Under this
 *   reading, emergence is gradual diffusion, not a threshold event, and
 *   institutional measurement is a lagging indicator rather than the origin.
 *   This reading does not adjudicate whether the first_held_reading's
 *   institutional-bearer threshold or the m4_m5_collapse_reading's
 *   measurement-artifact claim is correct — those are separate constraints in
 *   the same kernel family, evaluated independently, each with its own
 *   epsilon.
 *
 * KEY AGENTS:
 *   - early_electronic_payment_engineers: agenda_setter, built the technical substrate ahead of institutional recognition
 *   - clearinghouse_technologists: agenda_setter/beneficiary, normalized electronic settlement in practice
 *   - monetary_theorists_of_dematerialization: beneficiary, theoretical vindication
 *   - cash_dependent_populations: payer, excluded from the infrastructure being quietly built
 *   - unbanked_communities: payer, structurally excluded from the substrate itself
 *   - central_bank_statisticians: excluded from this reading's origin account, contest their placement
 *   - economic_historians: observer, primary audience for the diffusion-dating claim
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__became_thinkable_reading, 0.28).
domain_priors:suppression_score(electronic_money_emergence__became_thinkable_reading, 0.15).
domain_priors:theater_ratio(electronic_money_emergence__became_thinkable_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(electronic_money_emergence__became_thinkable_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__became_thinkable_reading, rope).
narrative_ontology:human_readable(electronic_money_emergence__became_thinkable_reading, "Digital Money's Conceptual-Diffusion Emergence (Became-Thinkable Reading)").
narrative_ontology:topic_domain(electronic_money_emergence__became_thinkable_reading, "economic_history/monetary_theory/technology_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__became_thinkable_reading, 'cba173b0-a1da-4741-a9a3-a6f6380231f1').
narrative_ontology:cs_kernel_codification('cba173b0-a1da-4741-a9a3-a6f6380231f1', distributed).
narrative_ontology:cs_authority_grounding('cba173b0-a1da-4741-a9a3-a6f6380231f1', distributed).
narrative_ontology:cs_reading_relation('cba173b0-a1da-4741-a9a3-a6f6380231f1', electronic_money_emergence__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('cba173b0-a1da-4741-a9a3-a6f6380231f1', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('cba173b0-a1da-4741-a9a3-a6f6380231f1', foundational, emergence_is_diffusion_not_event).
narrative_ontology:cs_axiom_status(emergence_is_diffusion_not_event, holdable).
narrative_ontology:cs_axiom_grounding('cba173b0-a1da-4741-a9a3-a6f6380231f1', emergence_is_diffusion_not_event, empirically_contingent).
narrative_ontology:cs_axiom('cba173b0-a1da-4741-a9a3-a6f6380231f1', secondary, measurement_lags_practice).
narrative_ontology:cs_axiom_status(measurement_lags_practice, holdable).
narrative_ontology:cs_axiom_grounding('cba173b0-a1da-4741-a9a3-a6f6380231f1', measurement_lags_practice, empirically_contingent).
narrative_ontology:cs_reference_frame('cba173b0-a1da-4741-a9a3-a6f6380231f1', engineering_and_theoretical_practice_as_origin).
narrative_ontology:cs_drift_state('cba173b0-a1da-4741-a9a3-a6f6380231f1', post_statistical_codification_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cba173b0-a1da-4741-a9a3-a6f6380231f1', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, early_electronic_payment_engineers).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, clearinghouse_technologists).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, cash_dependent_populations).
narrative_ontology:constraint_victim(electronic_money_emergence__became_thinkable_reading, unbanked_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Built interbank telegraphic transfer systems, magnetic-stripe processing, and ledger automation in the 1950s-1970s, treating dematerialized value transfer as an engineering problem to be solved rather than a monetary category awaiting official recognition. Their working prototypes and internal protocols preceded any statistical agency's decision to count 'electronic money' as a distinct aggregate.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, early_electronic_payment_engineers, agenda_setter,
    moderate, generational, mobile, national).

% Operated interbank settlement networks (e.g., early automated clearing houses) and normalized batch electronic settlement among institutions well before central banks formally distinguished electronic balances from physical currency in their statistics. Their operational practice made the conceptual possibility socially thinkable among bankers years before regulators noticed.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, clearinghouse_technologists, agenda_setter,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__became_thinkable_reading, clearinghouse_technologists, beneficiary).

% Economists and philosophers of money who argued, independent of any regulatory count, that money's essence was always a claim/ledger relationship rather than a physical token — electronic money's emergence vindicated their long-standing theoretical position. They benefit reputationally and intellectually from dating emergence to conceptual availability rather than institutional recognition.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, monetary_theorists_of_dematerialization, beneficiary,
    moderate, civilizational, arbitrage, global).

% Populations without stable banking access whose transactional lives remained physical-currency-based while institutions and infrastructure quietly reorganized around electronic settlement decades before regulators, policy, or financial products caught up to serve them. The gap between conceptual/technical availability and actual access to the new infrastructure fell on them as delayed inclusion, not benefit.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, cash_dependent_populations, payer,
    powerless, biographical, trapped, national).

% Communities structurally excluded from the banking relationships electronic money required as a substrate. As the conceptual and technical possibility diffused among institutions, unbanked communities did not merely lag adoption — the infrastructure being built around them was never designed with their access as a design constraint, deepening exclusion even as 'money' silently became electronic elsewhere in the economy.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, unbanked_communities, payer,
    powerless, generational, trapped, national).

% The agencies that eventually built M4/M5-style aggregates to count electronic money are, under this reading, latecomers documenting a phenomenon that had already emerged in practice. Their measurement apparatus is not the origin of the constraint this story describes; they are excluded from the emergence event itself, only entering once diffusion had already occurred. They would object to being read as merely retrospective — see the sibling m4_m5_collapse_reading.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, central_bank_statisticians, excluded,
    institutional, generational, analytical, national).

% Study the archival record of engineering practice, banking correspondence, and theoretical writing to locate when dematerialized money became conceptually and socially available, independent of when any statistical agency began counting it. They are the primary audience for the delta this reading claims: emergence as diffusion, not threshold.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__became_thinkable_reading, economic_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Names a shared reference point for dating monetary innovation so that historical and theoretical claims about 'when digital money began' can be evaluated by evidence of conceptual and technical availability rather than by whichever institution's bookkeeping happened to notice first.
% TRANSFER_FUNCTION: Moves interpretive authority over monetary history from institutional statisticians toward engineers, theorists, and archival historians; moves little in the way of material resources but reallocates credit for the innovation and shapes which populations count as included from the start versus catching up later.
% ABSENT_VOICES: Cash-dependent and unbanked populations have no voice in dating the emergence of the constraint that reorganized value transfer around them; central bank statisticians are structurally excluded from this reading's account of origin, since their measurement work postdates the phenomenon by this reading's own claim, and they would contest their placement as after-the-fact.
% DISAPPEARANCE_RATIONALE: If this reading of emergence vanished, the historical record of engineering and theoretical practice would remain unchanged, but the interpretive claim that anchors electronic money's 'true' origin to conceptual diffusion rather than institutional measurement would lose its warrant — historians and theorists dispute whether this matters for anything beyond dating conventions, while excluded populations' material situation would be unaffected either way.
% FOUNDING_PROBLEM: Existing monetary histories dated 'electronic money' to whichever statistical category (M4, M5, or similar) an institution first codified, which made the innovation appear to begin decades after practitioners had already been building and using dematerialized settlement systems — this reading was built to correct that anachronism by locating emergence at the point of conceptual and technical thinkability.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians working from banking archives and telecommunications records outside any central bank's own institutional history corroborate that clearinghouse automation and magnetic-stripe processing substantially preceded formal statistical recognition; central bank statisticians themselves (an outside-the-beneficiary-set source) dispute the framing, arguing that without measurement there is no way to distinguish genuine emergence from unmeasured background noise — this is precisely the contest the sibling m4_m5_collapse_reading formalizes.
narrative_ontology:disappearance_verdict(electronic_money_emergence__became_thinkable_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__became_thinkable_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__became_thinkable_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__became_thinkable_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__became_thinkable_reading_tests).
:- end_tests(electronic_money_emergence__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 by 2000) because this reading describes a genuine coordination/conceptual innovation with real diffusion costs, not a coercive extraction mechanism — the beneficiaries (engineers, technologists, theorists) gain credit and reputational vindication, not rents extracted from a captive population. However, extraction is non-zero and rising because the infrastructure that diffused conceptually and technically was not built with unbanked/cash-dependent populations as a design constraint, so as electronic settlement became normalized among institutions, those populations bore a slow-accumulating cost of exclusion from a transition they had no voice in. Suppression is low (0.15) because nothing coercive prevented cash-dependent populations from using electronic money — they were simply not served, which is a different mechanism than suppression proper. Theater ratio is low but rising slightly, reflecting the gradual institutionalization of practices that were originally purely functional engineering solutions.
 *
 * DIRECTIONALITY LOGIC:
 *   Engineers, technologists, and theorists sit near the beneficiary end: they collect credit, professional validation, and the ability to date the innovation to their own labor rather than a regulator's ledger. Cash-dependent and unbanked populations sit near the target end despite doing nothing to trigger the constraint — their directionality is high not because they were extracted from directly, but because the infrastructure reorganized the economy around dematerialized settlement without their access being designed in, producing a diffuse, generational cost of exclusion. Central bank statisticians are excluded rather than positioned as beneficiary or payer under this reading, since the reading's core claim is that their measurement work is not where emergence happened.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading resists mislabeling the emergence of electronic money as pure institutional fiat (which would treat measurement as constitutive) or as pure coordination innocent of exclusion (which would erase the unbanked/cash-dependent cost). By naming both a real coordination function (diffusion of a technically superior settlement technology) and a real, if diffuse, cost borne by populations outside the diffusion network, the story avoids collapsing into either a mountain (no one benefits, it just happened) or a snare (someone deliberately extracted). The rope classification records that this was substantially a genuine coordination achievement with an under-examined access externality, not a coercive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_became_thinkable,
    'Is it more structurally accurate to date electronic money''s emergence to the point of conceptual/technical thinkability (this reading), to a specific first institutional bearer (first_held_reading), or to treat ''emergence'' as an artifact retroactively constructed by the M4/M5 statistical distinction (m4_m5_collapse_reading)?',
    'Comparative archival work: trace whether engineering/theoretical practice (this reading''s evidence base) demonstrably preceded any locatable first-bearer event, and whether pre-statistical practitioners treated the phenomenon as already money (supporting this reading) versus whether contemporary sources show no coherent category existed until statisticians defined one (supporting the collapse reading).',
    'If archival evidence shows practitioners had no stable concept of ''electronic money'' as a monetary category prior to statistical codification, this reading''s epsilon and beneficiary structure would need substantial revision toward the m4_m5_collapse_reading''s account; if a clean first-bearer event is locatable with sharp before/after discontinuity, the first_held_reading would be favored as more precise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_choice_became_thinkable, conceptual, 'Which kernel reading of electronic money emergence best fits the historical record — routes the committer contest to an omega per Rule 2.').

omega_variable(
    diffusion_boundary_ambiguity,
    'Where exactly does ''conceptual and technical thinkability'' begin, given that partial forms of dematerialized value transfer (telegraphic transfer, book-entry clearing) predate even the 1950s engineering wave this story centers on?',
    'Historical study of 19th and early 20th century clearing and telegraphic transfer systems to determine whether they constitute genuine precedents for the same conceptual claim or a structurally distinct, non-continuous phenomenon.',
    'If genuine continuity exists back to 19th-century clearing systems, this story''s interval start (1950) understates the diffusion process and its true origin recedes further, potentially weakening the story''s implicit contrast with the m4_m5_collapse_reading''s late-20th-century measurement focus.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffusion_boundary_ambiguity, empirical, 'Whether the diffusion process this reading describes has a coherent starting boundary or recedes indefinitely.').

omega_variable(
    exclusion_cost_measurability,
    'Can the diffuse cost borne by unbanked and cash-dependent populations from being excluded-by-design from the dematerializing settlement infrastructure be measured independently of retrospective narrative, or is it only visible in hindsight?',
    'Comparative study of financial inclusion metrics in regions/periods where electronic settlement infrastructure diffused versus regions where it did not, controlling for other development factors.',
    'If the exclusion cost is measurable and attributable specifically to the diffusion pattern this reading describes, the extractiveness trajectory authored here is empirically supportable; if the cost is indistinguishable from general financial-development lag, the victim declaration may overstate this reading''s extractive component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_cost_measurability, empirical, 'Whether the exclusion cost attributed to this reading''s diffusion account is separable from general financial development patterns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__became_thinkable_reading, 1950, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t1950, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(elec_tr_t1960, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(elec_tr_t1970, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1970, 0.13).
narrative_ontology:measurement(elec_tr_t1980, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(elec_tr_t1990, electronic_money_emergence__became_thinkable_reading, theater_ratio, 1990, 0.19).
narrative_ontology:measurement(elec_tr_t2000, electronic_money_emergence__became_thinkable_reading, theater_ratio, 2000, 0.22).

% Extraction over time
narrative_ontology:measurement(elec_be_t1950, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1950, 0.12).
narrative_ontology:measurement(elec_be_t1960, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1960, 0.15).
narrative_ontology:measurement(elec_be_t1970, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1970, 0.19).
narrative_ontology:measurement(elec_be_t1980, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1980, 0.23).
narrative_ontology:measurement(elec_be_t1990, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 1990, 0.26).
narrative_ontology:measurement(elec_be_t2000, electronic_money_emergence__became_thinkable_reading, base_extractiveness, 2000, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(electronic_money_emergence__became_thinkable_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__became_thinkable_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__became_thinkable_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__first_held_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__became_thinkable_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the electronic_money_emergence kernel. became_thinkable_reading (this file) dates emergence to conceptual/technical availability in engineering and theoretical practice, with epsilon ~0.28 reflecting a genuine but imperfect coordination achievement with a diffuse exclusion externality. first_held_reading dates emergence to a specific institutional bearer holding dematerialized currency in distinguishable form — a threshold-event account with a different beneficiary/victim structure keyed to the specific institution involved. m4_m5_collapse_reading denies any pre-measurement fact of emergence at all, treating the entire category as a statistical artifact of the M4/M5 distinction — under that reading epsilon is authored around the extraction embedded in retroactive categorization itself, not diffusion costs. The three do not share an epsilon because they are not measuring the same referent: this reading's referent is practitioner activity, first_held_reading's referent is a specific custodial event, and m4_m5_collapse_reading's referent is the statistical apparatus itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
