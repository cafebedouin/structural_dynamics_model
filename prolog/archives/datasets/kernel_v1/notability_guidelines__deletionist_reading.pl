% ============================================================================
% CONSTRAINT STORY: notability_guidelines__deletionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_notability_guidelines__deletionist_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: notability_guidelines__deletionist_reading
 *   human_readable: Wikipedia Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)
 *   domain: digital_commons/platform_governance/knowledge_infrastructure
 *
 * SUMMARY:
 *   The Wikipedia Notability Guidelines represent a constitutive choice about
 *   how an open knowledge commons maintains quality without total editorial
 *   review. The deletionist reading interprets notability as a necessary
 *   epistemic filter: articles about non-verifiable subjects, vanity topics,
 *   or insufficiently-established concepts threaten the encyclopedia's
 *   credibility and consume volunteer editorial resources. From this
 *   perspective, deletion is coordination, not extraction — it solves the
 *   collective action problem of distinguishing valuable knowledge from
 *   noise. However, the same structural mechanism appears differently from
 *   other vantage points. Marginalized knowledge holders experience
 *   notability thresholds as systematic exclusion. Local history, non-Western
 *   scholarship, indigenous knowledge, and other epistemically valuable but
 *   locally-sourced material fall below thresholds calibrated to mainstream
 *   academic publishing. The deletionist reading naturalizes this exclusion
 *   as epistemic necessity; the inclusionist reading reframes it as
 *   institutional choice. The constraint instantiates a fundamental tension
 *   in platform governance: quality control requires boundaries, but
 *   boundaries embed power asymmetries. This JSON models the deletionist
 *   reading only — the reading that sees notability as coordination mechanism
 *   preserving commons integrity. The sibling readings (inclusionist: lower
 *   thresholds, expanded knowledge types; deliberative: participatory
 *   threshold-setting) are separate constraints with different ε values and
 *   different beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Encyclopedia Readership: Primary beneficiary (powerful/mobile) — benefits from quality curation without bearing moderation burden; can exit to alternative encyclopedias but values Wikipedia's coordination
 *   - Knowledge Commons Integrity: Primary beneficiary (abstract/N/A) — preserved by preventing vandalism, spam, original research; no agent but represented in editorial community's goals
 *   - Marginalized Subject Contributors: Secondary victim (moderate/constrained) — face systematic barriers from reliable-source bias, language bias, non-Western knowledge penalties; constrained because Wikipedia's reach is valuable but exclusion costly
 *   - Local and Niche Knowledge Holders: Tertiary victim (powerless/trapped) — fall below notability thresholds; trapped because no alternative platform reaches comparable audiences for local knowledge
 *   - Wikipedia Editorial Community: Institutional beneficiary (institutional/arbitrage) — experiences notability as coordination mechanism enabling resource allocation and moderation decisions; can arbitrage across deletion categories
 *   - Notability Guidelines as Institutional Artifact: Piton phenomenon (institutional/arbitrage) — maintains through institutional inertia; deletion culture, training practices, Wikimedia awards sustain guidelines despite declining functional value
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent governance choice as epistemic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(notability_guidelines__deletionist_reading, 0.32).
domain_priors:suppression_score(notability_guidelines__deletionist_reading, 0.38).
domain_priors:theater_ratio(notability_guidelines__deletionist_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(notability_guidelines__deletionist_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(notability_guidelines__deletionist_reading, rope).
narrative_ontology:human_readable(notability_guidelines__deletionist_reading, "Wikipedia Notability Guidelines as Epistemic Quality Filter (Deletionist Reading)").
narrative_ontology:topic_domain(notability_guidelines__deletionist_reading, "digital_commons/platform_governance/knowledge_infrastructure").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(notability_guidelines__deletionist_reading, 'c478aa42-093e-4e57-a79b-1b957a2af4cc').
narrative_ontology:cs_kernel_codification('c478aa42-093e-4e57-a79b-1b957a2af4cc', formalized).
narrative_ontology:cs_authority_grounding('c478aa42-093e-4e57-a79b-1b957a2af4cc', practice).
narrative_ontology:cs_interpretation_layer_present('c478aa42-093e-4e57-a79b-1b957a2af4cc').
narrative_ontology:cs_reading_relation('c478aa42-093e-4e57-a79b-1b957a2af4cc', notability_guidelines__inclusionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c478aa42-093e-4e57-a79b-1b957a2af4cc', notability_guidelines__deliberative_reading, influences).
narrative_ontology:cs_axiom('c478aa42-093e-4e57-a79b-1b957a2af4cc', foundational, notability_as_epistemic_necessity).
narrative_ontology:cs_axiom_status(notability_as_epistemic_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c478aa42-093e-4e57-a79b-1b957a2af4cc', notability_as_epistemic_necessity, empirically_contingent).
narrative_ontology:cs_axiom('c478aa42-093e-4e57-a79b-1b957a2af4cc', secondary, marginalization_is_justified_exclusion).
narrative_ontology:cs_axiom_status(marginalization_is_justified_exclusion, holdable).
narrative_ontology:cs_axiom_grounding('c478aa42-093e-4e57-a79b-1b957a2af4cc', marginalization_is_justified_exclusion, instrumental).
narrative_ontology:cs_reference_frame('c478aa42-093e-4e57-a79b-1b957a2af4cc', signal_preservation_through_curation).
narrative_ontology:cs_drift_state('c478aa42-093e-4e57-a79b-1b957a2af4cc', contemporary_wikipedia_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c478aa42-093e-4e57-a79b-1b957a2af4cc', '').
narrative_ontology:cs_kernel_id(notability_guidelines__deletionist_reading, notability_guidelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, encyclopedia_readership).
narrative_ontology:constraint_beneficiary(notability_guidelines__deletionist_reading, knowledge_commons_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: READERSHIP / QUALITY PRESERVATION (ROPE) — The notability filter solves a genuine coordination problem: distinguishing signal (verifiable, encyclopedic content) from noise (vanity, original research, advertising). Readers benefit from curation without bearing extraction costs. Exit option is mobile because readers can migrate to other encyclopedias or secondary sources, but Wikipedia's coordination is valuable.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED SUBJECT CONTRIBUTORS (TANGLED ROPE) — Non-Western, indigenous, LGBTQ+, disability, women's history, and other underrepresented subjects face systematic barriers to notability. Constrained by reliable-source requirements (Western academic publishing bias), non-English language penalties, and verifiability thresholds calibrated to mainstream topics. Receives some benefit from Wikipedia's global reach but bears disproportionate extraction cost through systemic exclusion. Exit is costly: abandoning Wikipedia means losing platform for otherwise-unmapped knowledge.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: LOCAL AND NICHE KNOWLEDGE HOLDERS (SNARE) — From the deletionist frame, highly local subjects (village histories, regional figures, specialized communities) fall below notability thresholds and face deletion. Trapped by Wikipedia's gatekeeping — no alternative platform reaches comparable audiences for local knowledge. Maximum extraction: valuable epistemic contributions are permanently removed; knowledge holders have no recourse. The notability filter, under deletionist enforcement, becomes a suppression mechanism for non-canonical knowledge.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: WIKIPEDIA EDITORIAL COMMUNITY (ROPE) — Administrators and deletion review committees experience notability as coordination mechanism: it enables them to manage vandalism, spam, and low-quality articles without unlimited moderation burden. The filter provides a structuring principle for editorial decisions. Arbitrage exit: editors can shift resource allocation across deletion categories without leaving the platform. Low extraction experienced because editors benefit from the coordination function itself.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NOTABILITY GUIDELINES AS INSTITUTIONAL ARTIFACT (PITON) — The guidelines have become substantially performative over their 20-year evolution. Deletion discussions invoke notability criteria but often route on unstated encyclopedic-judgment calls. Theater_ratio high because: (a) reliable-source requirement masks editorial preference for mainstream sources; (b) 'general audience interest' is never operationalized; (c) historical bias toward already-covered topics creates self-reinforcing canonicity. The ritual persists through institutional inertia — editors trained in deletion culture, Wikimedia Foundation grants awards for deletion reviews — despite declining functional coordination value.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, notability is an inherent epistemic requirement: any knowledge commons must filter signal from noise, and some quality standard is mathematically necessary to prevent commons tragedy. This perspective sees notability as natural law — immutable structural requirement. However, the constraint's metrics (low extractiveness 0.32, moderate suppression 0.38, low theater 0.45) and identified beneficiaries reveal this is institutional choice, not natural law. The deletionist reading naturalizes a contingent governance decision.
constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(notability_guidelines__deletionist_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(notability_guidelines__deletionist_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(notability_guidelines__deletionist_reading, TR),
    TR >= 0.70.

:- end_tests(notability_guidelines__deletionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.32): Low-to-moderate. The deletionist reading emphasizes notability's coordination function — it solves the genuine problem of maintaining encyclopedia quality without unlimited editorial review. However, it is not zero extraction because the filter does exclude legitimate knowledge from the global platform. The moderate value reflects that the constraint does both: enables coordination and creates barriers. The measurement trend (0.28 → 0.30 → 0.32) shows slow accumulation of extraction pressure as Wikipedia ages and deletion becomes more systematized. Suppression (0.38): Moderate. Barriers include reliable-source requirements (which disfavor non-Western sources), English-language bias, verification thresholds (which disadvantage emerging topics and non-academic knowledge), and deletion-culture enforcement. These are real but not total — alternatives exist (Wikimedia Commons, sister projects, external documentation), and some marginalized knowledge does survive notability scrutiny. Theater ratio (0.45): Moderate-low. Lower than the quality-maintenance narrative might suggest because some substantive editorial judgment drives notability decisions. However, theater is present: 'general audience interest,' 'encyclopedic value,' and verifiability standards are interpreted flexibly and reflect editorial consensus rather than objective criteria. The rising trend (0.30 → 0.38 → 0.45) indicates increasing performativity as deletion discussions accumulate and editors apply precedent rather than re-evaluating criteria. Theater rises because enforcement becomes ritualized — the decision framework persists through institutional practice even as its functional rationale evolves.
 *
 * PERSPECTIVAL GAP:
 *   The deletionist reading produces a narrow perspectival gap because the coordinating parties (readership, editorial community) experience low extraction, while the excluded parties (marginalized subjects, local knowledge) are positioned outside the coordination mechanism. The gap is not between negotiating parties — it is between those who benefit from coordination and those excluded from it. The piton perspective reveals that notability enforcement is becoming increasingly performative: deletion discussions invoke criteria but often route on aesthetic/canonical judgments. The mountain perspective risks naturalizing editorial choice as epistemic necessity. The tangled-rope perspective (marginalized subjects) shows the constraint is not pure coordination — it extracts value (knowledge exclusion) alongside coordinating (quality maintenance). The deletionist reading assumes the snare perspective (local knowledge holders) represents invalid entries correctly excluded; the inclusionist reading would argue the snare is a systematic injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from each agent's relationship to the extraction/coordination flow. Readership (beneficiary, mobile exit) has low d — they benefit from quality curation and can exit to alternatives but choose Wikipedia. Editorial community (beneficiary, arbitrage exit) has very low d — they experience notability as enabling their coordinating function. Marginalized subjects (victim, constrained exit) have moderate-high d — they bear extraction costs through systematic barriers but have structural mobility (could publish elsewhere) at significant cost. Local knowledge holders (victim, trapped exit) have very high d — they bear full extraction cost and face insurmountable barriers to alternative platforms with comparable reach. The canonical fallback (moderate power → d≈0.65) was overridden by beneficiary/victim declarations: beneficiaries are explicitly identified, victims are explicitly identified, and their exit options are specified. The directionality chain computes: beneficiary + mobile → low d; victim + constrained → moderate-high d; victim + trapped → very high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The deletionist reading resolves the potential mandatrophy by declaring notability as pure coordination (Rope in the primary perspectives). The extractiveness value (0.32) is below the snare threshold (0.46), which forestalls the mandatrophy anxiety: 'Is this coordination or extraction?' The constraint is claimed as Rope because it genuinely coordinates (readership benefits, editorial management enabled). However, the marginal perspectives reveal real extraction: the snare perspective (local knowledge) and tangled-rope perspective (marginalized subjects) show that the coordination mechanism excludes knowledge systematically. The mandatrophy is resolved by acknowledging multiple truths: notability IS coordination from the beneficiary perspective, AND notability IS extraction from the victim perspective. The constraint is genuinely Rope for those it benefits; Snare for those it excludes. The deletionist reading emphasizes the coordination function; the inclusionist reading would emphasize the extraction function. Both are structurally correct — they describe the same mechanism from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    notability_threshold_measurability,
    'Is notability (as defined by the guidelines) objectively measurable or inherently subjective editorial judgment?',
    'Inter-rater reliability study: Have multiple independent editors apply notability criteria to 100 disputed articles. Measure agreement rates. If agreement < 70%, criteria are subjective proxies for editorial preference.',
    'If measurable: coordination function is real (Rope from all perspectives). If subjective: notability criteria are cover story for discretionary gatekeeping (Snare/Tangled Rope from marginalized perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(notability_threshold_measurability, empirical, 'Whether notability criteria are objectively measurable or subjective judgment').

omega_variable(
    alternate_filtration_mechanism_sufficiency,
    'Could alternative quality-filter mechanisms (tagging, probabilistic visibility, community-based curation) achieve comparable signal/noise ratio without deletionism?',
    'Comparative analysis: measure vandalism/spam persistence in tagged-but-not-deleted articles vs deleted articles; assess user-perceived quality in forums with algorithmic vs editorial filtering.',
    'If alternatives sufficient: notability is coordination choice, not necessity (Rope with alternatives visible). If alternatives insufficient: notability deletion is irreducible (Mountain). Deletionist reading is valid only if alternatives fail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternate_filtration_mechanism_sufficiency, empirical, 'Whether alternative filtration mechanisms could replace deletion-based quality control').

omega_variable(
    marginalization_mechanism_in_notability,
    'Are systematic biases toward Western/English/mainstream sources embedded in the notability criteria themselves, or are they artifacts of reliable-source availability?',
    'Comparative deletion rate analysis: measure deletion rates for (a) topics with strong non-Western source bases (e.g., traditional medicine, indigenous history) vs (b) equivalent-notability Western topics. If rates differ, criteria embed systematic bias.',
    'If embedded: notability is Snare for marginalized knowledge (extraction mechanism). If artifact: notability is neutral tool mis-applied (Tangled Rope — coordination with systemic bias). This determines whether deletionist reading is defensible or naturalizes exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginalization_mechanism_in_notability, empirical, 'Whether notability criteria embed systematic biases toward Western/English knowledge').

omega_variable(
    reading_contestation_nature,
    'Is the disagreement between deletionist and inclusionist readings a dispute about notability thresholds (quantitative), or about the epistemic legitimacy of different knowledge types (qualitative)?',
    'Discourse analysis: map deletion discussions onto (a) threshold disputes (''is X notable enough?'') vs (b) legitimacy disputes (''should local history count as encyclopedic?''). If >60% of contested cases are legitimacy disputes, readings are not commensurate.',
    'If quantitative: readings coexist — both parties can accept higher thresholds (influences relation, not forecloses). If qualitative: readings may foreclose (deletionist denies legitimacy of inclusionist''s knowledge categories). This determines reading_relations value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_contestation_nature, conceptual, 'Whether reading dispute is about notability thresholds or epistemic legitimacy of knowledge types').

omega_variable(
    false_summit_epistemic_necessity,
    'Is the mountain classification (notability as natural law) justified by actual commons-degradation dynamics, or is it post-hoc naturalization of editorial preference?',
    'Historical reconstruction: trace the conceptual origins of notability (early Wikipedia deletion policies, Nupedia influence, editorial culture evolution). Assess whether current deletionism follows from empirical commons-tragedy dynamics or from inherited editorial priors.',
    'If justified: mountain classification valid (natural law of knowledge commons). If naturalization: this reading is false-summit candidate — deletionist framing masks institutional choice. This determines whether the analytical perspective''s mountain classification should trigger FSM evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_epistemic_necessity, conceptual, 'Whether notability as natural law is justified by commons-degradation dynamics or is naturalization of editorial preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(notability_guidelines__deletionist_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(notab_del_tr_t0, notability_guidelines__deletionist_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(notab_del_tr_t7, notability_guidelines__deletionist_reading, theater_ratio, 7, 0.38).
narrative_ontology:measurement(notab_del_tr_t14, notability_guidelines__deletionist_reading, theater_ratio, 14, 0.45).

% Extraction over time
narrative_ontology:measurement(notab_del_be_t0, notability_guidelines__deletionist_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(notab_del_be_t7, notability_guidelines__deletionist_reading, base_extractiveness, 7, 0.3).
narrative_ontology:measurement(notab_del_be_t14, notability_guidelines__deletionist_reading, base_extractiveness, 14, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(notab_del_su_t0, notability_guidelines__deletionist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(notab_del_su_t7, notability_guidelines__deletionist_reading, suppression_requirement, 7, 0.36).
narrative_ontology:measurement(notab_del_su_t14, notability_guidelines__deletionist_reading, suppression_requirement, 14, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(notability_guidelines__deletionist_reading, information_standard).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__inclusionist_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, notability_guidelines__deliberative_reading).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, reliable_sources_bias_toward_mainstream).
narrative_ontology:affects_constraint(notability_guidelines__deletionist_reading, wikipedia_deletion_as_epistemic_violence).

% DUAL FORMULATION NOTE:
% The notability guidelines kernel decomposes into three structurally distinct constraint stories with different ε values: (1) deletionist_reading (ε=0.32, Rope) — notability as coordination; (2) inclusionist_reading (ε≈0.55, Tangled Rope, separate file) — notability as systematic exclusion of marginalized knowledge; (3) deliberative_reading (ε≈0.40, Scaffold, separate file) — notability with participatory threshold-setting. Each reading instantiates a different structural interpretation of the same governance kernel. The deletionist reading emphasizes coordination; the inclusionist reading emphasizes extraction; the deliberative reading proposes sunset/reform. All three are live readings held by different factions in Wikipedia governance. The trio forms a constraint family: inclusion of beneficiaries/victims in deletionist reading enables FSM detection if the analysis reveals the coordination function is weaker than claimed.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
