% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press–Reformation Mutual Shaping Dynamic
 *   domain: historical/technological/religious
 *
 * SUMMARY:
 *   The printing press (c. 1450) and the Protestant Reformation (1517+)
 *   co-evolved in a bidirectional causal loop: the press's material
 *   affordances — standardization, fixity, combinability, and scalable
 *   reproduction — enabled reformers to bypass ecclesiastical censorship and
 *   synchronize dissent across Europe; simultaneously, reformers' urgent
 *   demand for vernacular Bibles, polemical pamphlets, and confessional
 *   literature drove printers to innovate in typography, distribution
 *   networks, and business models. This mutual shaping dynamic peaked during
 *   the Reformation era (1517–1648) and transformed as confessional
 *   boundaries hardened and state churches established licensed print
 *   regimes. The constraint is neither a Mountain (technology determining
 *   history) nor a pure Rope (voluntary coordination) but a Scaffold: an
 *   enabling structure with a historical sunset, whose coordination function
 *   (mass dissemination of reform ideas) was genuine but whose extraction
 *   function (confessional control, censorship, print monopoly rents) grew
 *   over time.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.45).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.55).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.45).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press–Reformation Mutual Shaping Dynamic").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "historical/technological/religious").

domain_priors:requires_active_enforcement(press_reformation_causation__mutual_shaping).
narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'e5c64dc1-d6af-4fca-b0c5-31814248a3de').
narrative_ontology:cs_kernel_codification('e5c64dc1-d6af-4fca-b0c5-31814248a3de', distributed).
narrative_ontology:cs_authority_grounding('e5c64dc1-d6af-4fca-b0c5-31814248a3de', practice).
narrative_ontology:cs_interpretation_layer_present('e5c64dc1-d6af-4fca-b0c5-31814248a3de').
narrative_ontology:cs_reading_relation('e5c64dc1-d6af-4fca-b0c5-31814248a3de', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('e5c64dc1-d6af-4fca-b0c5-31814248a3de', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('e5c64dc1-d6af-4fca-b0c5-31814248a3de', foundational, bidirectional_causation_press_reformation).
narrative_ontology:cs_axiom_status(bidirectional_causation_press_reformation, holdable).
narrative_ontology:cs_axiom_grounding('e5c64dc1-d6af-4fca-b0c5-31814248a3de', bidirectional_causation_press_reformation, empirically_contingent).
narrative_ontology:cs_axiom('e5c64dc1-d6af-4fca-b0c5-31814248a3de', foundational, press_as_scaffold_with_sunset).
narrative_ontology:cs_axiom_status(press_as_scaffold_with_sunset, holdable).
narrative_ontology:cs_axiom_grounding('e5c64dc1-d6af-4fca-b0c5-31814248a3de', press_as_scaffold_with_sunset, empirically_contingent).
narrative_ontology:cs_reference_frame('e5c64dc1-d6af-4fca-b0c5-31814248a3de', reformation_era_mutual_shaping).
narrative_ontology:cs_drift_state('e5c64dc1-d6af-4fca-b0c5-31814248a3de', post_westphalia_print_regime, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5c64dc1-d6af-4fca-b0c5-31814248a3de', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, protestant_reformers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, literate_lay_publics).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_church_authority).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_scribes).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, censored_authors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, vernacular_printers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, political_authorities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Luther, Calvin, Zwingli and their networks used the press to bypass ecclesiastical gatekeepers, distributing vernacular Bibles and polemics at scale. They gained unprecedented reach for their theology but became dependent on printers' technical choices and commercial rhythms. Exit meant returning to manuscript circulation or oral preaching — possible but drastically reduced impact.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, protestant_reformers, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, protestant_reformers, beneficiary).

% Printers in Wittenberg, Basel, Geneva, and Antwerp found a massive new market for Reformation texts. They profited from the surge in demand but faced censorship raids, confiscation of stock, and the need to navigate shifting political protections. Their exit options included moving to freer cities or switching to safer commercial printing — feasible but costly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_printers, beneficiary,
    moderate, biographical, mobile, regional).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, vernacular_printers, payer).

% The Roman Curia, local bishops, and the Inquisition lost monopoly control over religious discourse. They responded with the Index Librorum Prohibitorum, pre-publication licensing, and the Council of Trent's doctrinal hardening. Their exit from the constraint would mean abandoning doctrinal unity claims — structurally impossible for the institution's self-conception.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_church_authority, payer,
    institutional, civilizational, constrained, continental).

% Urban artisans, merchants, and minor nobility gained direct access to scripture and theological debate in their own languages. This enabled new forms of piety and political identity but also exposed them to persecution, confessional violence, and the burden of choosing between competing authorities. Exit meant conformity or exile.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, literate_lay_publics, beneficiary,
    moderate, biographical, constrained, continental).

% Monastic and commercial scribes saw their livelihood collapse as printed books replaced hand-copied manuscripts. Some transitioned to proofreading or compositing in print shops; most were displaced without equivalent alternatives. Their structural position offered no organized resistance.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, manuscript_scribes, payer,
    powerless, immediate, trapped, local).

% Radical reformers (Anabaptists, Spiritualists), humanist critics of both sides, and heterodox thinkers found their works banned, burned, or published only anonymously abroad. The press that enabled mainstream reformers also enabled their suppression through centralized censorship apparatus. Exit meant silence, exile, or martyrdom.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, censored_authors, payer,
    powerless, immediate, trapped, local).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, censored_authors, excluded).

% Princes and city councils (German electors, Swiss cantons, Scandinavian monarchs) exploited the press to legitimize state churches, seize church lands, and standardize administrative languages. They bore costs of religious war and social disruption but gained unprecedented tools for bureaucratic centralization and ideological control. Their exit options included switching confessions (cuius regio) or suppressing print entirely — both exercised in practice.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, political_authorities, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, political_authorities, payer).

% Modern historians (Eisenstein, Febvre & Martin, Pettegree, Rubin) analyze the co-evolution from archival evidence. They neither collect rents nor bear costs from the historical constraint but their interpretive frameworks shape how the mutual shaping thesis is received in contemporary discourse.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press solved the coordination problem of synchronizing theological dissent across linguistic and political boundaries: reformers in Saxony, Switzerland, and the Low Countries could read each other's works, cite common authorities, and build a shared movement without ever meeting.
% TRANSFER_FUNCTION: The arrangement transferred interpretive authority from Latin-literate clergy to vernacular readers; transferred economic value from manuscript culture to print capitalism; transferred political legitimacy from universal church to territorial rulers who could claim to protect 'true religion' in their domains.
% ABSENT_VOICES: Illiterate peasant majorities (80-90% of population) who experienced the Reformation through oral preaching and ritual change rather than reading; women excluded from university training and guild printing; non-European printing traditions (Ottoman, Chinese, Korean) that developed under entirely different constraints; Jewish communities subject to both Christian censorship and their own rabbinic print controls.
% DISAPPEARANCE_RATIONALE: If the mutual shaping dynamic vanished — i.e., if the press had not enabled Reformation spread, or reformers had not driven press adaptation — the Reformation likely remains a local academic dispute; print culture develops as a humanist scholarly tool rather than a mass medium; the modern public sphere and nation-state vernaculars emerge differently or later.
% FOUNDING_PROBLEM: How to disseminate religious reform ideas beyond the local manuscript networks that ecclesiastical authorities could easily monitor and suppress, while simultaneously creating a sustainable economic basis for producing and distributing vernacular texts at scale.
% FOUNDING_PROBLEM_CORROBORATION: Catholic historians (Jedin, Hubert) and secular historians (Febvre, Eisenstein) agree the founding problem — breaking the Church's information monopoly — was solved by the 1550s. The mutual shaping dynamic that solved it (press enabling reform, reform driving press innovation) ceased to be the primary driver once confessional boundaries hardened and state churches established their own print regimes.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, world_rearranges).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__mutual_shaping_tests).
:- end_tests(press_reformation_causation__mutual_shaping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.20 to 0.55 as confessional states monetize print monopolies and enforce censorship, then falls to 0.45 as the Peace of Westphalia stabilizes the system. Suppression peaks at 0.70 during the height of confessional warfare (1580s) when both sides deploy pre-publication licensing and the Index. Theater ratio remains low (0.10–0.30) because the press's coordination function (actual dissemination) was real throughout, not performative. Accessibility collapse at 0.50 reflects that manuscript culture and oral transmission persisted as alternatives but became marginal for authoritative texts. Resistance at 0.70 captures the Catholic Church's sustained counter-reformation, radical reformers' persistence, and political authorities' strategic switching.
 *
 * PERSPECTIVAL GAP:
 *   From the reformer's seat, the press is a Rope (voluntary coordination enabling their message). From the Catholic authority's seat, it is a Snare (extraction of doctrinal control via enforced censorship). From the printer's seat, it is a Tangled Rope (coordination of production/distribution with extraction via monopoly privileges). The engine computes this divergence; the claimed_type 'scaffold' represents the system-level historical phase, not any single seat's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformers and printers are structural beneficiaries (d ≈ 0.2–0.3): they gained reach, revenue, and institutional footholds. Catholic authority and manuscript scribes are targets (d ≈ 0.8–0.9): they lost monopoly control and livelihoods. Lay publics sit near symmetric (d ≈ 0.5): genuine access gain but also new burdens of choice and persecution. Political authorities are agenda-setters with arbitrage exit (d ≈ 0.15): they shaped the constraint to their advantage and could switch confessions. Censored authors are trapped victims (d ≈ 0.95). The engine computes per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (breaking the Church's information monopoly) was solved by the 1550s — the mutual shaping dynamic that solved it became a stable confessional print regime. The constraint persists in transformed shape (state-licensed print, censorship apparatus) but its original coordination function is complete. This is mandatrophy resolved: the scaffold served its transitional purpose and was replaced by successor constraints (confessional print monopolies, later public sphere emergence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Is the mutual_shaping reading of press_reformation_causation a single constraint with stable ε, or does it conflate distinct constraints (press-as-enabler vs. reformers-as-drivers) that should be decomposed per ε-invariance?',
    'Test whether ε for ''press enables Reformation spread'' differs from ε for ''Reformation shapes press development'' when measured from the same stakeholder seats. If they differ, decompose into two constraint stories linked via network.affects_constraints.',
    'If decomposed, each sub-constraint gets its own classification (likely scaffold for press-as-enabler, tangled_rope for reformers-shaping-press). If unified, the single ε = 0.45 represents a genuine bidirectional system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Whether the mutual shaping label covers one ε-invariant constraint or two structurally distinct ones.').

omega_variable(
    symmetry_of_causation,
    'Was the causation truly bidirectional and symmetrical, or did the press''s material affordances (standardization, fixity, combinability) constrain reformers more than reformers constrained the press?',
    'Compare the rate of press technology change (type design, distribution networks, business models) driven by Reformation demand vs. the rate of theological change constrained by print''s material logic (e.g., need for authoritative editions, citation practices, vernacular standardization).',
    'If asymmetrical (press constrained reformers more), the constraint leans toward technological_determinism structurally. If symmetrical, mutual_shaping holds as a distinct reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(symmetry_of_causation, empirical, 'Directional balance of the bidirectional causation claim.').

omega_variable(
    scaffold_sunset_naturalness,
    'Does the mutual shaping dynamic have a genuine sunset (end of Reformation era) or does it persist in transformed form (modern media–social movement co-evolution)?',
    'Trace whether the structural relationship — new communication technology enables dissent, dissent shapes technology''s development — recurs with pamphlets→newspapers→radio→internet. If recurrent, the ''scaffold'' classification applies only to the historical instantiation, not the pattern.',
    'If recurrent pattern, the constraint is a piton (degraded scaffold) or a new scaffold instance per era. If historically unique, has_sunset_clause: true is accurate for this constraint_id.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scaffold_sunset_naturalness, conceptual, 'Whether the scaffold sunset is historical contingency or structural necessity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1517, 1648).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.1).
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1530, press_reformation_causation__mutual_shaping, theater_ratio, 1530, 0.15).
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1555, press_reformation_causation__mutual_shaping, theater_ratio, 1555, 0.25).
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1580, press_reformation_causation__mutual_shaping, theater_ratio, 1580, 0.3).
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1618, press_reformation_causation__mutual_shaping, theater_ratio, 1618, 0.25).
narrative_ontology:measurement(press_reformation_mutual_shaping_tr_t1648, press_reformation_causation__mutual_shaping, theater_ratio, 1648, 0.25).

% Extraction over time
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.2).
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1530, press_reformation_causation__mutual_shaping, base_extractiveness, 1530, 0.35).
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1555, press_reformation_causation__mutual_shaping, base_extractiveness, 1555, 0.5).
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1580, press_reformation_causation__mutual_shaping, base_extractiveness, 1580, 0.55).
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1618, press_reformation_causation__mutual_shaping, base_extractiveness, 1618, 0.5).
narrative_ontology:measurement(press_reformation_mutual_shaping_be_t1648, press_reformation_causation__mutual_shaping, base_extractiveness, 1648, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1517, press_reformation_causation__mutual_shaping, suppression_requirement, 1517, 0.3).
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1530, press_reformation_causation__mutual_shaping, suppression_requirement, 1530, 0.5).
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1555, press_reformation_causation__mutual_shaping, suppression_requirement, 1555, 0.65).
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1580, press_reformation_causation__mutual_shaping, suppression_requirement, 1580, 0.7).
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1618, press_reformation_causation__mutual_shaping, suppression_requirement, 1618, 0.65).
narrative_ontology:measurement(press_reformation_mutual_shaping_su_t1648, press_reformation_causation__mutual_shaping, suppression_requirement, 1648, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.03).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, confessional_print_monopolies).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, vernacular_standardization).

% DUAL FORMULATION NOTE:
% The press_reformation_causation kernel decomposes into three readings: technological_determinism (press as Mountain-like driver), strategic_deployment (press as Rope-like tool), and mutual_shaping (this constraint, press as Scaffold with bidirectional causation). Each has distinct ε: technological_determinism ε ≈ 0.1 (low extraction, high naturalness claim), strategic_deployment ε ≈ 0.3 (moderate extraction, coordination-focused), mutual_shaping ε = 0.45 (bidirectional, historical sunset). They form a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(press_reformation_causation__mutual_shaping, institutional, 0.15).
constraint_indexing:directionality_override(press_reformation_causation__mutual_shaping, organized, 0.25).
constraint_indexing:directionality_override(press_reformation_causation__mutual_shaping, moderate, 0.5).
constraint_indexing:directionality_override(press_reformation_causation__mutual_shaping, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
