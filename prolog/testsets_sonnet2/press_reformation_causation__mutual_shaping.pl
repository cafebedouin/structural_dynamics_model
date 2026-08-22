% ============================================================================
% CONSTRAINT STORY: press_reformation_causation__mutual_shaping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-17
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: press_reformation_causation__mutual_shaping
 *   human_readable: Printing Press as Co-Evolutionary Scaffold for Reformation Agency
 *   domain: history_of_technology/religious_history/media_studies
 *
 * SUMMARY:
 *   Between the introduction of movable-type printing in the mid-15th century
 *   and the confessionalized print regimes of 1600, the technical capacity of
 *   the press and the strategic goals of religious reformers developed in
 *   continuous feedback: printers adapted formats, pricing, and distribution
 *   to reformist demand, and reformers adapted their rhetorical and
 *   organizational strategies to what the printing infrastructure could
 *   actually deliver at scale. This story authors that co-evolutionary
 *   reading specifically — it is one of three readings of a contested kernel
 *   about press/Reformation causation, and it is deliberately NOT a story
 *   about the press as fixed technological cause (technological_determinism)
 *   nor about reformers as pure strategic users of a neutral tool
 *   (strategic_deployment). Under this reading, the press functioned as a
 *   scaffold: an enabling structure reformers reinforced through use, whose
 *   own subsequent development was shaped by that reinforcement, with a
 *   natural endpoint as the arrangement matured into stabilized confessional
 *   print institutions by 1600.
 *
 * KEY AGENTS:
 *   - reformist_printers: primary co-adaptive agent (organized/mobile) — modifies technology in response to demand and is modified by it
 *   - vernacular_literate_laity: demand-side co-shaper (moderate/constrained) — benefits from and drives content adaptation
 *   - early_protestant_princes: political co-shaper (powerful/mobile) — patronizes and directs technical investment
 *   - manuscript_copyists_guilds: displaced incumbent (organized/trapped) — bears the cost of the co-evolved arrangement's success
 *   - unlicensed_dissenting_printers: exploited-then-abandoned innovators (moderate/trapped) — used the same feedback loop but lacked institutional protection when it consolidated
 *   - catholic_censorship_authorities: outpaced regulator (institutional/constrained) — bears reputational and institutional cost of adaptation lag
 *   - media_historians: analytical observer (analytical/analytical) — reconstructs the interaction pattern without stake in either pole
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(press_reformation_causation__mutual_shaping, 0.28).
domain_priors:suppression_score(press_reformation_causation__mutual_shaping, 0.22).
domain_priors:theater_ratio(press_reformation_causation__mutual_shaping, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, extractiveness, 0.28).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(press_reformation_causation__mutual_shaping, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(press_reformation_causation__mutual_shaping, scaffold).
narrative_ontology:human_readable(press_reformation_causation__mutual_shaping, "Printing Press as Co-Evolutionary Scaffold for Reformation Agency").
narrative_ontology:topic_domain(press_reformation_causation__mutual_shaping, "history_of_technology/religious_history/media_studies").

narrative_ontology:has_sunset_clause(press_reformation_causation__mutual_shaping).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(press_reformation_causation__mutual_shaping, 'de99c6b0-6ba9-42c2-a433-5ffd3182b636').
narrative_ontology:cs_kernel_codification('de99c6b0-6ba9-42c2-a433-5ffd3182b636', distributed).
narrative_ontology:cs_authority_grounding('de99c6b0-6ba9-42c2-a433-5ffd3182b636', distributed).
narrative_ontology:cs_reading_relation('de99c6b0-6ba9-42c2-a433-5ffd3182b636', press_reformation_causation__technological_determinism, coexists_with).
narrative_ontology:cs_reading_relation('de99c6b0-6ba9-42c2-a433-5ffd3182b636', press_reformation_causation__strategic_deployment, coexists_with).
narrative_ontology:cs_axiom('de99c6b0-6ba9-42c2-a433-5ffd3182b636', foundational, technology_and_agency_are_co_constituted).
narrative_ontology:cs_axiom_status(technology_and_agency_are_co_constituted, holdable).
narrative_ontology:cs_axiom_grounding('de99c6b0-6ba9-42c2-a433-5ffd3182b636', technology_and_agency_are_co_constituted, conventional).
narrative_ontology:cs_axiom('de99c6b0-6ba9-42c2-a433-5ffd3182b636', foundational, neither_technical_affordance_nor_intention_has_causal_priority).
narrative_ontology:cs_axiom_status(neither_technical_affordance_nor_intention_has_causal_priority, holdable).
narrative_ontology:cs_axiom_grounding('de99c6b0-6ba9-42c2-a433-5ffd3182b636', neither_technical_affordance_nor_intention_has_causal_priority, conventional).
narrative_ontology:cs_reference_frame('de99c6b0-6ba9-42c2-a433-5ffd3182b636', co_adaptive_emergence_framework).
narrative_ontology:cs_drift_state('de99c6b0-6ba9-42c2-a433-5ffd3182b636', post_book_history_turn, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('de99c6b0-6ba9-42c2-a433-5ffd3182b636', '').
narrative_ontology:cs_kernel_id(press_reformation_causation__mutual_shaping, press_reformation_causation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, reformist_printers).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, vernacular_literate_laity).
narrative_ontology:constraint_beneficiary(press_reformation_causation__mutual_shaping, early_protestant_princes).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, manuscript_copyists_guilds).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, unlicensed_dissenting_printers).
narrative_ontology:constraint_victim(press_reformation_causation__mutual_shaping, catholic_censorship_authorities).
narrative_ontology:constraint_vindicates(press_reformation_causation__mutual_shaping, co_construction_of_technology_and_social_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Print shops in cities like Wittenberg, Basel, and Strasbourg adapted press capacity to produce cheap pamphlets and vernacular Bibles at speed. They did not just use a fixed tool — they modified typefaces, formats, and distribution networks in response to reformist demand, and that adaptation in turn made the press a different kind of object than it had been for indulgence-printing or humanist scholarship.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, reformist_printers, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, reformist_printers, beneficiary).

% Newly-literate or semi-literate townspeople gained access to scripture and polemical literature in their own language. Their appetite for such material shaped what printers chose to produce, which shaped what reformers chose to write, in a feedback loop neither side fully controlled.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, vernacular_literate_laity, beneficiary,
    moderate, biographical, constrained, regional).

% Territorial rulers who broke with Rome used printed propaganda to consolidate legitimacy and patronized printers directly. Their political interests co-shaped which technical improvements (paper supply, typeface standardization, censorship-evasion formats) got invested in.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, early_protestant_princes, beneficiary,
    powerful, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, early_protestant_princes, agenda_setter).

% Scribal guilds whose economic function was displaced as the press-reform feedback loop accelerated demand for printed rather than copied texts. They had no comparable capacity to redirect their trade and largely disappeared as a profession within two generations.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, manuscript_copyists_guilds, payer,
    organized, biographical, trapped, regional).

% Smaller printers producing radical or heterodox material (Anabaptist tracts, peasant polemics) rode the same co-evolutionary wave but lacked princely protection. They bore the costs when the press-reform alliance hardened into official orthodoxy and turned enforcement against exactly the improvisational uses that had made the technology generative in the first place.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, unlicensed_dissenting_printers, payer,
    moderate, biographical, trapped, local).

% Ecclesiastical censors found their existing licensing apparatus, built for a world of scarce manuscript production, structurally outpaced by a technology whose social use had already been reshaped by reformers before censorship regimes could adapt. They bore reputational and institutional costs as the co-evolved arrangement outran their capacity to respond, though they eventually built the Index and pre-publication licensing in reply.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, catholic_censorship_authorities, payer,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(press_reformation_causation__mutual_shaping, catholic_censorship_authorities, agenda_setter).

% Contemporary and later scholars reconstruct the interaction pattern between press capacity and reformist strategy from surviving print runs, correspondence, and guild records, without a stake in either technology's inevitability or reformers' pure agency.
narrative_ontology:constraint_stakeholder(press_reformation_causation__mutual_shaping, media_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(press_reformation_causation__mutual_shaping, diffuse).
narrative_ontology:fixing_cost_class(press_reformation_causation__mutual_shaping, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The press-reform relationship solved a genuine mutual problem: reformers needed scalable distribution for vernacular theology and polemic, and printers needed reliable high-volume demand to justify capital investment in type, paper supply chains, and shop expansion. Neither could achieve their goals at the scale realized without the other's adaptive response.
% TRANSFER_FUNCTION: Legitimacy, literacy capacity, and market demand moved from vernacular readers and princely patrons into an emerging technical-religious infrastructure; economic function moved away from scribal guilds and unlicensed printers as the co-evolved arrangement stabilized around larger, protected print operations.
% ABSENT_VOICES: Scribal guild members and radical unlicensed printers experienced the same co-evolutionary dynamic as displacement rather than emergence, but their objections survive mostly as guild petitions and trial records rather than as participants in the historiographical conversation, which has been dominated by studying the winning combination of press and mainstream reform.
% DISAPPEARANCE_RATIONALE: Historians of technology and religious historians disagree on how much would change if the specific co-evolutionary trajectory had not occurred: technological-determinist readings hold the press would have produced comparable religious upheaval regardless of which movement seized it; strategic-deployment readings hold reformers would have found comparable alternative channels (preaching networks, manuscript circulation) absent the press. This reading holds that the SPECIFIC form the Reformation took — its speed, its vernacular character, its regional variation — depended on the particular feedback loop between press capacity and reformist use, such that removing either side would rearrange the outcome's shape even if some analogous religious contestation persisted.
% FOUNDING_PROBLEM: Neither side set out to solve a single defined problem; the arrangement emerged from two independent projects (movable-type commercial printing, and theological reform seeking wider audiences) discovering that their independent adaptations reinforced each other faster than either anticipated.
% FOUNDING_PROBLEM_CORROBORATION: Media historians and book-history scholars (outside both the printing trade's own guild memory and confessional Reformation historiography) attest, via surviving print-run and typeface records, that the mutual-adaptation dynamic was real but time-bound to the 16th-century technical and religious conditions; no living party benefits from maintaining the arrangement today, which is why this reading treats the founding problem as historically resolved rather than live.
narrative_ontology:disappearance_verdict(press_reformation_causation__mutual_shaping, contested).
narrative_ontology:founding_problem_status(press_reformation_causation__mutual_shaping, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(press_reformation_causation__mutual_shaping, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(press_reformation_causation__mutual_shaping, 'none', 1).
narrative_ontology:epsilon_provenance(press_reformation_causation__mutual_shaping, 0.28, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored low-to-moderate (0.08 rising to 0.28) because the core dynamic — technology and movement shaping each other through use — is genuinely coordinative for most of the interval; it rises only as the arrangement matures into institutionalized print regimes that begin extracting value asymmetrically from displaced scribal labor and unlicensed printers. Theater ratio stays low throughout (0.02 to 0.12) because this reading concerns a real functional co-adaptation, not a performative one — there is little ceremonial maintenance involved in movable type and pamphlet production finding their mutual fit. Suppression (0.22) and accessibility_collapse (0.35) are moderate rather than high: alternatives to the specific co-evolved trajectory (oral preaching networks, manuscript circulation, purely secular print markets) persisted throughout the period and were not fully foreclosed, which is exactly what distinguishes a scaffold reading from a mountain reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Reformist printers and princely patrons sit near the beneficiary end because they captured the gains of the co-adaptive process — expanded markets, political legitimacy, durable institutional footing. Vernacular laity sit closer to symmetric: real benefit from access, but their reading preferences were also being shaped and to some extent narrowed by what the co-evolved system made available. Manuscript guilds and unlicensed printers sit near the target end: they participated in or were adjacent to the same feedback loop but were structurally unable to convert participation into durable position once the arrangement consolidated. Catholic censorship authorities are a payer seat of a different kind — not extracted from economically, but structurally disadvantaged by adaptation lag relative to a faster-moving co-evolutionary system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — two independent projects (commercial printing, theological reform) discovering mutual reinforcement — is authored as dead: the specific 16th-century conditions (scarce vernacular access, unconsolidated censorship, a genuinely novel print technology) no longer hold, and no living party has an interest in re-litigating whether the co-evolution 'still needs to happen.' This keeps the reading from being mistaken for an ongoing extractive arrangement; it is a scaffold whose transitional period has closed, consistent with the sunset-clause declaration.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_reformation_without_press,
    'Would a functionally similar religious reformation have occurred through preaching networks and manuscript circulation absent the printing press, or was the press a necessary enabling condition for the Reformation''s specific scale and speed?',
    'Comparative study of pre-print heterodox movements (Hussites, Lollards) against post-print reform movements controlling for other variables (literacy rates, urbanization, political fragmentation) to isolate the press''s marginal contribution.',
    'If a comparable reformation was highly likely regardless of print technology, this reading''s claim that the co-evolved trajectory shaped the outcome''s specific SHAPE (not its occurrence) is strengthened relative to strong determinism; if print was strictly necessary for any reformation at that scale, the technological_determinism reading gains ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_reformation_without_press, conceptual, 'Whether printing was a necessary condition for Reformation-scale religious upheaval or merely shaped its particular form.').

omega_variable(
    locus_of_agency_disagreement,
    'Where, structurally, does the disagreement between the three kernel readings actually live — in the historical record itself, or in how much causal weight different disciplinary traditions (history of technology vs. intellectual history vs. media studies) assign to technological affordance versus human intention?',
    'Cross-disciplinary review tracing which specific archival evidence each reading treats as decisive, to determine whether the dispute is empirical (about what happened) or conceptual (about how causation should be apportioned between structure and agency).',
    'If the dispute is substantially conceptual, no additional archival evidence will resolve it and all three readings remain permanently coexisting framings; if substantially empirical, targeted archival work (e.g., on how quickly printers technically adapted to reformist demand versus reformers adapting rhetoric to press capacity) could shift consensus toward one reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(locus_of_agency_disagreement, conceptual, 'Whether the three-reading kernel dispute is resolvable by evidence or is a standing framing disagreement.').

omega_variable(
    beneficiary_of_co_construction_narrative,
    'Does the mutual_shaping reading itself carry a disciplinary beneficiary — media studies and STS scholarship that gains explanatory territory by favoring co-construction narratives over either pure determinism or pure agency accounts?',
    'Examine whether media-studies and STS citation patterns and grant funding disproportionately favor co-evolutionary framings compared to history-of-technology or church-history subfields, controlling for the underlying evidence quality of each framing.',
    'If co-construction framings carry disciplinary advantage independent of evidentiary merit, this reading''s own scaffold/beneficiary structure (reformist_printers etc.) has an analytical-observer analogue: media_historians as a discipline may itself be a low-order beneficiary of favoring this reading, which would not invalidate the reading but would contextualize its adoption.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_of_co_construction_narrative, conceptual, 'Whether the mutual-shaping historiographical framework has its own disciplinary beneficiaries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(press_reformation_causation__mutual_shaping, 1450, 1600).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pres_tr_t1450, press_reformation_causation__mutual_shaping, theater_ratio, 1450, 0.02).
narrative_ontology:measurement(pres_tr_t1480, press_reformation_causation__mutual_shaping, theater_ratio, 1480, 0.03).
narrative_ontology:measurement(pres_tr_t1517, press_reformation_causation__mutual_shaping, theater_ratio, 1517, 0.05).
narrative_ontology:measurement(pres_tr_t1540, press_reformation_causation__mutual_shaping, theater_ratio, 1540, 0.09).
narrative_ontology:measurement(pres_tr_t1560, press_reformation_causation__mutual_shaping, theater_ratio, 1560, 0.11).
narrative_ontology:measurement(pres_tr_t1600, press_reformation_causation__mutual_shaping, theater_ratio, 1600, 0.12).

% Extraction over time
narrative_ontology:measurement(pres_be_t1450, press_reformation_causation__mutual_shaping, base_extractiveness, 1450, 0.08).
narrative_ontology:measurement(pres_be_t1480, press_reformation_causation__mutual_shaping, base_extractiveness, 1480, 0.1).
narrative_ontology:measurement(pres_be_t1517, press_reformation_causation__mutual_shaping, base_extractiveness, 1517, 0.15).
narrative_ontology:measurement(pres_be_t1540, press_reformation_causation__mutual_shaping, base_extractiveness, 1540, 0.25).
narrative_ontology:measurement(pres_be_t1560, press_reformation_causation__mutual_shaping, base_extractiveness, 1560, 0.28).
narrative_ontology:measurement(pres_be_t1600, press_reformation_causation__mutual_shaping, base_extractiveness, 1600, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(press_reformation_causation__mutual_shaping, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(press_reformation_causation__mutual_shaping, information_standard).
narrative_ontology:boltzmann_floor_override(press_reformation_causation__mutual_shaping, 0.05).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__technological_determinism).
narrative_ontology:affects_constraint(press_reformation_causation__mutual_shaping, press_reformation_causation__strategic_deployment).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the press_reformation_causation kernel. technological_determinism authors the press as a fixed causal mountain (near-zero extraction, high accessibility_collapse, emerges_naturally-style inevitability framing) whose beneficiary declarations would trigger FSM scrutiny if authored. strategic_deployment authors reformers as strategic agents deploying a neutral tool (closer to a pure rope: reformers as beneficiaries via genuine coordination, technology itself carrying no independent causal weight). This mutual_shaping story sits structurally between them: it authors moderate extraction rising over time, a genuine but time-bound (sunset-clause) coordination function, and a scaffold classification reflecting bidirectional causation neither sibling reading captures. All three share the same underlying historical episode but are NOT the same constraint — each has independently authored epsilon and metrics per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
