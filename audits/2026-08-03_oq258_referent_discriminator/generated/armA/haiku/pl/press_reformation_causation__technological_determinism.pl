% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__technological_determinism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_press_reformation_causation__technological_determinism, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: press_reformation_causation__technological_determinism
 *   human_readable: Printing Press Causation of Reformation (Technological Determinism Reading)
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Under the technological determinism reading of the printing press's role
 *   in the Reformation, the printing press is the causal prime mover.
 *   Gutenberg's mechanical reproduction technology made censorship impossible
 *   at scale, made vernacular scripture inevitable, and made the
 *   Reformation's core outcomes (mass literacy, direct text access,
 *   institutional authority fragmentation) structurally unavoidable
 *   regardless of human intention, strategy, or resistance. The Roman
 *   Catholic Church's institutional monopoly on scriptural interpretation was
 *   rendered obsolete by technological fact, not defeated by reformist
 *   agency. Reformers are downstream beneficiaries of exogenous technological
 *   capacity, not upstream strategists deploying a tool. This reading claims
 *   the constraint is a mountain — a natural fact of technological capability
 *   that allows no alternatives once grasped. The other readings
 *   (strategic_deployment, mutual_shaping) are alternative interpretations of
 *   the same historical kernel, not rival facts.
 *
 * KEY AGENTS:
 *   - Printing technology itself — the causal prime mover under determinism
 *   - Roman Catholic Church — institutional authority structure rendered obsolete by technical capability
 *   - Protestant reformers — downstream beneficiaries of exogenous technological capacity
 *   - European literacy expansion — inevitable consequence of printing's material capabilities
 *   - Manuscript scribal guilds — economically rendered obsolete by mechanical reproduction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__technological_determinism, 0.15).
domain_priors:suppression_score(press_reformation_causation__technological_determinism, 0.08).
domain_priors:theater_ratio(press_reformation_causation__technological_determinism, 0.02).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, extractiveness, 0.15).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, theater_ratio, 0.02).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__technological_determinism, mountain).
narrative_ontology:human_readable(press_reformation_causation__technological_determinism, "Printing Press Causation of Reformation (Technological Determinism Reading)").
narrative_ontology:topic_domain(press_reformation_causation__technological_determinism, "history_of_technology/religious_history/media_studies").

domain_priors:emerges_naturally(press_reformation_causation__technological_determinism).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__technological_determinism, 'd0d65764-0fec-4f82-9b00-08ec23c9f435').
narrative_ontology:cs_kernel_codification('d0d65764-0fec-4f82-9b00-08ec23c9f435', distributed).
narrative_ontology:cs_authority_grounding('d0d65764-0fec-4f82-9b00-08ec23c9f435', diffuse_epistemic).
narrative_ontology:cs_reading_relation('d0d65764-0fec-4f82-9b00-08ec23c9f435', press_reformation_causation__strategic_deployment, forecloses).
narrative_ontology:cs_reading_relation('d0d65764-0fec-4f82-9b00-08ec23c9f435', press_reformation_causation__mutual_shaping, forecloses).
narrative_ontology:cs_axiom('d0d65764-0fec-4f82-9b00-08ec23c9f435', foundational, technology_determines_outcomes).
narrative_ontology:cs_axiom_status(technology_determines_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('d0d65764-0fec-4f82-9b00-08ec23c9f435', technology_determines_outcomes, empirically_contingent).
narrative_ontology:cs_axiom('d0d65764-0fec-4f82-9b00-08ec23c9f435', foundational, human_agency_subordinate_to_capability).
narrative_ontology:cs_axiom_status(human_agency_subordinate_to_capability, holdable).
narrative_ontology:cs_axiom_grounding('d0d65764-0fec-4f82-9b00-08ec23c9f435', human_agency_subordinate_to_capability, deontological).
narrative_ontology:cs_reference_frame('d0d65764-0fec-4f82-9b00-08ec23c9f435', printing_press_as_exogenous_fact).
narrative_ontology:cs_drift_state('d0d65764-0fec-4f82-9b00-08ec23c9f435', reformation_consolidation_1550, gap(stable, minor, false)).
narrative_ontology:cs_created_at('d0d65764-0fec-4f82-9b00-08ec23c9f435', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__technological_determinism, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__technological_determinism, protestant_reformers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, roman_catholic_church).
narrative_ontology:constraint_victim(press_reformation_causation__technological_determinism, manuscript_scribes_guild).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The mechanical reproduction of text at scale. Under this reading, the technology is the causal prime mover: its capabilities (speed, cost reduction, vernacular accessibility, distribution reach, resistance to suppression) made certain outcomes inevitable regardless of human intention or resistance.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, printing_technology, observer,
    analytical, generational, analytical, global).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, printing_technology).

% The institutional authority structure that had maintained monopoly control over scriptural interpretation through manuscript scarcity and Latin gatekeeping. Under technological determinism, the Church's resistance to printing and censorship efforts were futile — the technology's capacity exceeded institutional suppression capability. The Church bears the cost of losing interpretive monopoly.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, roman_catholic_church, payer,
    institutional, civilizational, trapped, continental).

% Receive exogenous technological capacity (cheap vernacular printed scripture, rapid distribution) that makes their core message (direct access to scripture, lay reading in native language, challenge to institutional mediation) structurally inevitable rather than strategically chosen. They are downstream beneficiaries of technological determinism, not upstream agents deploying a tool.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, protestant_reformers, beneficiary,
    moderate, generational, mobile, continental).

% The rising supply of readable texts in vernacular languages. Under determinism, printing makes mass literacy inevitable; reformist theology is the downstream consequence of that capability, not the upstream driver.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, european_literacy_expansion, beneficiary,
    analytical, generational, analytical, continental).
narrative_ontology:stakeholder_non_agent(press_reformation_causation__technological_determinism, european_literacy_expansion).

% Professional scribes whose labor becomes economically redundant as mechanical reproduction scales. The technology renders their skillset obsolete independent of institutional choice or market strategy.
narrative_ontology:constraint_stakeholder(press_reformation_causation__technological_determinism, manuscript_scribes_guild, payer,
    organized, biographical, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None — this reading denies coordination function. Under technological determinism, the printing press is not coordinating a collective-action problem; it is exogenously imposing a new material fact on all parties simultaneously. The Church cannot coordinate with reformers to prevent the Reformation because the technology's inevitability precludes negotiation.
% TRANSFER_FUNCTION: None — there is no transfer under determinism. The technology creates new material possibilities; parties do not transfer resources to or from each other. The Church loses interpretive monopoly, but not through extraction; through technological obsolescence.
% ABSENT_VOICES: Scribal guilds and manuscript producers are structurally excluded by the deterministic logic: their objections and resistance are treated as powerless against technological inevitability. The reading itself silences them by denying their agency matters.
% DISAPPEARANCE_RATIONALE: Under technological determinism, if the printing press had never been invented, the Reformation would not have occurred — the technology is the causal prime mover. Its disappearance would leave the world unchanged in its capacity to generate reformation-like movements, because the causal factor (technological capacity for mass reproduction) would be absent.
% FOUNDING_PROBLEM: Why did the Reformation occur when it did, in the specific form it took (mass vernacular scripture, rapid geographic spread, institutional challenge to monopoly)? Technological determinism answers: printing made these outcomes inevitable by removing the material scarcity that had maintained institutional control.
% FOUNDING_PROBLEM_CORROBORATION: Historians of technology (Elizabeth Eisenstein, Adrian Johns) and some media theorists support deterministic causation narratives. However, leading Reformation historians (Andrew Pettegree, Peter Marshall, Christopher Haigh) explicitly reject determinism in favor of mutual shaping or strategic deployment, arguing that printing enabled but did not determine reformist outcomes — the same technological capacity was used for Catholic catechesis, political propaganda, and entertainment, and reformers' ideological innovations shaped printing's development. No consensus exists outside the deterministic school; the strongest corroboration comes from within media-determinist circles.
narrative_ontology:disappearance_verdict(press_reformation_causation__technological_determinism, world_unchanged).
narrative_ontology:founding_problem_status(press_reformation_causation__technological_determinism, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__technological_determinism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__technological_determinism, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__technological_determinism, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(press_reformation_causation__technological_determinism_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, ExtMetricName, E),
    domain_priors:suppression_score(press_reformation_causation__technological_determinism, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(press_reformation_causation__technological_determinism),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(press_reformation_causation__technological_determinism, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(press_reformation_causation__technological_determinism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   This reading authors LOW extractiveness (0.15 at interval end), LOW suppression (0.08), and negligible theater (0.02) because under determinism the constraint is NOT an extraction mechanism — it is natural law. The printing press's capacity to reproduce text at scale and low cost is a physical/technical fact, not a social arrangement anyone designed to extract benefit. The Church's loss of interpretive monopoly is NOT extraction; it is obsolescence. The reformers' gain is NOT transfer from a beneficiary; it is benefiting from new material possibility. The measurements show low values precisely because the reading insists this is NOT a human-designed constraint but technological inevitability. Accessibility collapse is HIGH (0.92) because once the technology exists, alternatives to printed vernacular scripture are materially unavailable — you cannot un-invent the press. Resistance is near-zero (0.03) because resistance to natural law is futile; the Church's censorship efforts are treated as powerless against technological determinism. The measurement series track the technology's diffusion from 1450 (pre-printing) through 1550 (mature printing infrastructure), showing how the constraint's presence rises but extractiveness remains low because the reading denies extraction occurs.
 *
 * PERSPECTIVAL GAP:
 *   Under technological determinism, there is NO perspectival gap — all parties experience the same material fact (the printing press's capacity) the same way. The Church experiences it as institutional obsolescence; reformers experience it as enabling; scribes experience it as labor displacement — but all are experiencing the SAME exogenous technological reality, not a negotiated constraint they could reshape. The engine would compute directionality for reformers as near-beneficiary (d ~0.1) and for the Church as near-target (d ~0.9), but the reading insists both are simply downstream of technological fact. If the reading is correct (technology determines), all parties should converge on the same classification (mountain). If they diverge substantially, the reading is false — agency matters, and the constraint is tangled_rope or snare, not mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   Under determinism, directionality is near-trivial because the reading denies the constraint is a constructed arrangement. Printing technology is not an agenda-setter in the sense of intentionally designing extraction; it is an exogenous fact. The Church and reformers have no meaningful choice — both are passive recipients of technological capability. If forced to assign directionality: reformers are beneficiaries (d near 0.0) because they receive enabling capacity without cost; the Church is a target (d near 1.0) because it experiences loss of monopoly. But the reading insists these are not directionalities of extraction — they are directionalities of material displacement. The reformers benefit not because someone chose to benefit them, but because they happened to align with the technology's affordances. The Church suffers not because someone extracted from it, but because its institutional logic became obsolete. This is precisely why the constraint should classify as mountain if the reading is true: mountains have directionality in the sense that some agents align with natural law (benefit) and others do not (suffer), but no extraction occurs because no human choice designed the arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   Under technological determinism, mandatrophy is impossible by definition. Mandatrophy arises when a constraint's founding mandate (the problem it was built to solve) becomes obsolete but the constraint persists through institutional inertia. Printing was never built to solve any problem — it was invented as a technical capability. The Reformation was not the founding mandate of the printing press; it is a downstream consequence. Therefore, mandatrophy cannot attach to a constraint whose mandate is technological capability itself (capability doesn't become obsolete; it persists and compounds). If mandatrophy appears in the corpus data, it signals that the reading is false — that printing was actually a designed constraint (tangled_rope or snare) with a human mandate that later became hollow. The absence of mandatrophy under this reading is thus diagnostic: it confirms the reading's truth claim if borne out by historical data.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_reading,
    'Is the printing press''s causal role in the Reformation a natural law of technological capability (technology determines outcomes through material inevitability) or a constructed reading that benefits the reformers and media-determinist scholars by naturalizing what was actually contingent strategic deployment?',
    'Historical counterfactual analysis: what would have happened if printing had been available but reformers had strategically chosen NOT to use it for scripture distribution? If reformation-like movements emerged anyway, determinism fails. If the movement required intentional deployment, then agency precedes technology.',
    'If the reading is constructed rather than natural, the constraint shifts from mountain to tangled_rope: technology enables coordination, but human agents strategically exploit that enabling for extractive purposes (fragmenting Catholic authority, consolidating reformist power). The beneficiary/victim relationship becomes intentional, not technological inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_reading, conceptual, 'Whether technological determinism describes natural inevitability or is a constructed reading that naturalizes contingent human choices.').

omega_variable(
    suppression_futility_claim,
    'Did the Church''s censorship efforts genuinely fail due to printing''s technical superiority, or did they succeed in slowing and shaping the Reformation while printers and reformers strategically evaded them?',
    'Archival study of Catholic printing output, suppression tactics effectiveness, and counterfactual printing scenarios under different enforcement regimes (as occurred in some regions where Church authority remained stronger).',
    'If suppression genuinely failed due to technical inevitability, determinism holds and the Church''s resistance was futile. If suppression shaped outcomes — slowing spread, forcing innovation in smuggling, concentrating distribution in specific regions — then agency reasserts and the outcome is contingent, not determined.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_futility_claim, empirical, 'Whether Church censorship failed inevitably or was strategically evaded.').

omega_variable(
    reformer_agency_subordination,
    'Does technological determinism correctly characterize reformers as passive downstream beneficiaries of exogenous capacity, or does it erase their strategic intentionality in innovating printing techniques (woodcut imagery, layout design for persuasion, distribution networks) specifically for reformist goals?',
    'Intellectual and material history of printing: study of how reformers'' theological demands shaped the form and distribution of printed books, and whether the same technical capacity would have developed identically without reformist market demand.',
    'If reformers'' intentional shaping of printing technology''s development is substantial, determinism collapses into mutual shaping: technology and agency co-evolved. The constraint becomes tangled_rope (coordination of printing and theology) rather than pure technological mountain.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reformer_agency_subordination, conceptual, 'Whether reformers'' strategic innovation is erased by the deterministic reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__technological_determinism, 1450, 1550).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__technological_determinism, theater_ratio, 1450, 0.0).
narrative_ontology:measurement_basis(pres_tr_t1450, projected).
narrative_ontology:measurement(pres_tr_t1475, press_reformation_causation__technological_determinism, theater_ratio, 1475, 0.0).
narrative_ontology:measurement_basis(pres_tr_t1475, projected).
narrative_ontology:measurement(pres_tr_t1500, press_reformation_causation__technological_determinism, theater_ratio, 1500, 0.01).
narrative_ontology:measurement_basis(pres_tr_t1500, observed).
narrative_ontology:measurement(pres_tr_t1525, press_reformation_causation__technological_determinism, theater_ratio, 1525, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1525, observed).
narrative_ontology:measurement(pres_tr_t1550, press_reformation_causation__technological_determinism, theater_ratio, 1550, 0.02).
narrative_ontology:measurement_basis(pres_tr_t1550, observed).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__technological_determinism, base_extractiveness, 1450, 0.0).
narrative_ontology:measurement_basis(pres_be_t1450, projected).
narrative_ontology:measurement(pres_be_t1475, press_reformation_causation__technological_determinism, base_extractiveness, 1475, 0.02).
narrative_ontology:measurement_basis(pres_be_t1475, projected).
narrative_ontology:measurement(pres_be_t1500, press_reformation_causation__technological_determinism, base_extractiveness, 1500, 0.08).
narrative_ontology:measurement_basis(pres_be_t1500, observed).
narrative_ontology:measurement(pres_be_t1525, press_reformation_causation__technological_determinism, base_extractiveness, 1525, 0.14).
narrative_ontology:measurement_basis(pres_be_t1525, observed).
narrative_ontology:measurement(pres_be_t1550, press_reformation_causation__technological_determinism, base_extractiveness, 1550, 0.15).
narrative_ontology:measurement_basis(pres_be_t1550, observed).

% Suppression requirement over time
narrative_ontology:measurement(pres_su_t1450, press_reformation_causation__technological_determinism, suppression_requirement, 1450, 0.0).
narrative_ontology:measurement_basis(pres_su_t1450, projected).
narrative_ontology:measurement(pres_su_t1475, press_reformation_causation__technological_determinism, suppression_requirement, 1475, 0.02).
narrative_ontology:measurement_basis(pres_su_t1475, projected).
narrative_ontology:measurement(pres_su_t1500, press_reformation_causation__technological_determinism, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement_basis(pres_su_t1500, observed).
narrative_ontology:measurement(pres_su_t1525, press_reformation_causation__technological_determinism, suppression_requirement, 1525, 0.07).
narrative_ontology:measurement_basis(pres_su_t1525, observed).
narrative_ontology:measurement(pres_su_t1550, press_reformation_causation__technological_determinism, suppression_requirement, 1550, 0.08).
narrative_ontology:measurement_basis(pres_su_t1550, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__technological_determinism, global_infrastructure).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__strategic_deployment).
narrative_ontology:affects_constraint(press_reformation_causation__technological_determinism, press_reformation_causation__mutual_shaping).

% DUAL FORMULATION NOTE:
% The kernel press_reformation_causation decomposes into three structurally distinct constraint stories: technological_determinism (technology as prime mover, natural law), strategic_deployment (neutral tool, intentional exploitation), and mutual_shaping (co-evolutionary). Each has different ε, different beneficiary/victim structures, and different classification. The constraint family is linked via affects_constraints; each story names its siblings as alternative readings of the same kernel, not as rival measurements of a single constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
