% ============================================================================
% CONSTRAINT STORY: anthropological_record__indigenous_epistemology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__indigenous_epistemology_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: anthropological_record__indigenous_epistemology_reading
 *   human_readable: Anthropological Record as Indigenous Epistemology (Oral Tradition Reading)
 *   domain: epistemology/philosophy_of_science/anthropology
 *
 * SUMMARY:
 *   This constraint models the indigenous epistemology reading of the
 *   anthropological record — the claim that genealogical and spatial
 *   knowledge transmitted through oral tradition constitutes valid evidence
 *   of relational continuity with ancestors and place, independent of
 *   material corroboration or credentialing by academic institutions. This
 *   reading directly challenges the naturalist assumption that only written
 *   documents, dated artifacts, and genetic data count as legitimate records.
 *   The constraint is one reading of a contested kernel: the anthropological
 *   record itself (what counts, who interprets it, how truth is established).
 *   The indigenous epistemology reading asserts that indigenous communities
 *   have legitimate authority to narrate their own past through oral
 *   transmission; the naturalist reading insists on material evidence and
 *   scientific methodology; the creationist reading appeals to sacred text.
 *   These three readings cannot be harmonized within a single epistemic
 *   framework — they dispute what 'the record' is. The constraint exhibits a
 *   spectrum of institutional and community positions: indigenous knowledge
 *   keepers are powerless under naturalist dominance; indigenous sovereignty
 *   movements are organized but constrained; decolonial scholars enjoy
 *   institutional arbitrage; museums experience loss of epistemic authority;
 *   academic anthropology performs deference while preserving gatekeeping;
 *   the naturalist frame appears as immutable natural law but is revealed as
 *   a false summit grounded in institutional suppression.
 *
 * KEY AGENTS:
 *   - Indigenous Communities and Knowledge Keepers: Powerless/trapped in institutional frameworks. Claim authority over ancestral narratives through oral tradition. Bear suppression from requirements for written evidence or laboratory verification.
 *   - Indigenous Sovereignty Movements: Organized/constrained. Assert control over repatriation, remains curation, and interpretation. Benefit from reframing but face sustained legal and institutional resistance.
 *   - Decolonial and Applied Anthropologists: Institutional/arbitrage. Gain career advancement and methodological innovation through collaborative work recognizing oral tradition. See constraint as coordination problem with solutions (partnership models, co-authored publications).
 *   - Natural History Museums and Curators: Powerful/mobile institutional actors. Historically benefited from exclusive curatorial authority. Experience constraint as loss of epistemic gatekeeping. Performatively defer while preserving institutional control.
 *   - Academic Anthropology Discipline: Institutional/arbitrage. Maintains formal commitment to scientific materialism while performing 'community consultation.' Theater ratio rising: protocols (ethics boards, IRB approvals, consent forms) are increasingly performative without redistributing actual epistemic authority.
 *   - Naturalist Epistemology Framework: Analytical/universal perspective. Claims material evidence is the only valid record, treating scientific methodology as a natural law. Revealed as false summit: the 'natural law' is institutionally enforced suppression of alternative frameworks.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, 0.48).
domain_priors:suppression_score(anthropological_record__indigenous_epistemology_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__indigenous_epistemology_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__indigenous_epistemology_reading, tangled_rope).
narrative_ontology:human_readable(anthropological_record__indigenous_epistemology_reading, "Anthropological Record as Indigenous Epistemology (Oral Tradition Reading)").
narrative_ontology:topic_domain(anthropological_record__indigenous_epistemology_reading, "epistemology/philosophy_of_science/anthropology").

domain_priors:requires_active_enforcement(anthropological_record__indigenous_epistemology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__indigenous_epistemology_reading, 'e6cdc310-2b2e-4a71-badb-9103207ef890').
narrative_ontology:cs_kernel_codification('e6cdc310-2b2e-4a71-badb-9103207ef890', distributed).
narrative_ontology:cs_authority_grounding('e6cdc310-2b2e-4a71-badb-9103207ef890', extraction).
narrative_ontology:cs_interpretation_layer_present('e6cdc310-2b2e-4a71-badb-9103207ef890').
narrative_ontology:cs_reading_relation('e6cdc310-2b2e-4a71-badb-9103207ef890', anthropological_record__naturalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e6cdc310-2b2e-4a71-badb-9103207ef890', anthropological_record__creationist_reading, coexists_with).
narrative_ontology:cs_axiom('e6cdc310-2b2e-4a71-badb-9103207ef890', foundational, indigenous_community_epistemic_authority).
narrative_ontology:cs_axiom_status(indigenous_community_epistemic_authority, holdable).
narrative_ontology:cs_axiom_grounding('e6cdc310-2b2e-4a71-badb-9103207ef890', indigenous_community_epistemic_authority, deontological).
narrative_ontology:cs_axiom('e6cdc310-2b2e-4a71-badb-9103207ef890', secondary, oral_transmission_fidelity).
narrative_ontology:cs_axiom_status(oral_transmission_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('e6cdc310-2b2e-4a71-badb-9103207ef890', oral_transmission_fidelity, empirically_contingent).
narrative_ontology:cs_reference_frame('e6cdc310-2b2e-4a71-badb-9103207ef890', community_authority_over_ancestral_narrative).
narrative_ontology:cs_drift_state('e6cdc310-2b2e-4a71-badb-9103207ef890', contemporary_institutional_recognition, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e6cdc310-2b2e-4a71-badb-9103207ef890', '').
narrative_ontology:cs_kernel_id(anthropological_record__indigenous_epistemology_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, indigenous_communities).
narrative_ontology:constraint_beneficiary(anthropological_record__indigenous_epistemology_reading, oral_tradition_practitioners).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, credential_based_researchers).
narrative_ontology:constraint_victim(anthropological_record__indigenous_epistemology_reading, natural_history_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS KNOWLEDGE KEEPER (SNARE) — Bears suppression from external credentialing systems that deny oral tradition as legitimate knowledge. Trapped by institutional frameworks that require written evidence or laboratory verification. No exit from the extractive dynamic where their knowledge is treated as folklore unless validated by outside authorities.
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: INDIGENOUS SOVEREIGNTY MOVEMENT (TANGLED ROPE) — Organized actors asserting authority over ancestral remains and historical narratives. Benefits from reframing the record as indigenous property (coordination function: community authority, repatriation rights). Also bears costs: constrained by legal frameworks, requires sustained mobilization, faces institutional resistance. Mixed extraction and genuine coordination.
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DECOLONIAL SCHOLARS (ROPE) — Benefit from institutional legitimacy while advancing indigenous knowledge claims. See the constraint as coordination: bringing oral tradition into academic discourse creates new research opportunities, methodological innovation, and career advancement through collaborative work. Arbitrage exits available (publish in multiple venues, shift between disciplines).
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MUSEUM/INSTITUTIONAL CURATORS (SNARE, INSTITUTIONAL BENEFICIARY) — Powerful actors experiencing the constraint as a loss of authority. Collections management, exhibition narrative, and institutional prestige have historically rested on curator expertise and written provenance. The shift to recognizing indigenous oral tradition as equally valid undermines institutional gatekeeping. Mobile in theory but invested in preserving epistemic hierarchy. Classification reflects experienced loss of extraction capacity, not weakness of position.
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC ANTHROPOLOGY (PITON) — Maintains formal commitment to 'scientific objectivity' and material evidence while increasingly performing deference to indigenous authority. Theater ratio high: protocols for 'community consultation' and 'indigenous partnership' persist as ritual without redistribution of epistemic control. Discipline has absorbed the constraint without functional transformation of core authority structures.
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURALIST READING AS ALTERNATIVE CONSTRAINT (MOUNTAIN, FALSE SUMMIT CANDIDATE) — From a purely naturalist frame, only material evidence (archaeology, genetics, dated layers) constitutes valid knowledge about the past. Oral tradition is folklore, susceptible to invention and drift. This perspective treats scientific materialism as an immutable natural law. However, the structural data reveals this as a false summit: the naturalist reading itself benefits from institutional credentialing systems and excludes alternative epistemologies through suppression, not logic.
constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__indigenous_epistemology_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(anthropological_record__indigenous_epistemology_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__indigenous_epistemology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(anthropological_record__indigenous_epistemology_reading, TR),
    TR >= 0.70.

:- end_tests(anthropological_record__indigenous_epistemology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint exhibits significant extraction from indigenous communities (suppression of oral tradition as legitimate knowledge, requirement for credentialing through external systems, institutional gatekeeping over whose narratives count). However, extractiveness is not extreme because indigenous sovereignty movements have successfully organized and extracted institutional concessions (repatriation laws, consultation protocols, co-management of collections). The metric reflects the hybrid state: genuine suppression exists, but not total — some communities have leveraged political power to assert epistemic authority. Suppression (0.62): Moderate-high. High barriers to recognition: institutional credentialing requirements, scientific materialism as disciplinary norm, skepticism of oral transmission fidelity, legal frameworks that privilege written provenance and material evidence. But suppression is declining (t0=0.78 → t20=0.48) as legal frameworks recognize indigenous authority and decolonial methods gain disciplinary legitimacy. Theater ratio (0.58): Moderate. Academic protocols for 'community consultation,' 'ethical research partnerships,' and 'indigenous co-management' are increasingly ritualized without genuine redistribution of epistemic control. Museums have adopted repatriation policies while preserving narrative authority over remaining collections. The theater is rising (t0=0.42 → t20=0.58) as institutional deference becomes performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The six perspectives reveal a complete spectrum of institutional and epistemic positions. The indigenous knowledge keeper and sovereignty movement see snare and tangled rope (extraction with barriers). Decolonial scholars see rope (coordination and career benefit). Museums see loss of authority (snare for institutional gatekeepers). Anthropology discipline sees ritual maintenance (piton — performative protocols without functional change). The naturalist frame sees immutable law (mountain — false summit). The perspectival gap is maximized between indigenous powerless actors and institutional beneficiaries: indigenous communities experience suppression; researchers experience opportunity; museums experience threat; the discipline performs transformation while preserving hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   This reading's directionality differs fundamentally from the naturalist reading's perspective. From the indigenous epistemology frame, indigenous communities are victims bearing suppression; credentialed researchers and institutions are beneficiaries extracting authority. The beneficiary/victim declarations establish d values for each perspective: indigenous communities trapped/powerless → high d; researchers/institutions arbitrage/institutional → low d; organized sovereignty movements constrained/organized → moderate d. The engine derives these from structural position. The false summit mountain perspective (naturalist reading) has its own constraint story with its own ε and its own beneficiary/victim structure — which will reveal it as extractive, not natural law.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by resolving the classification ambiguity at the reading level: this reading instantiates tangled_rope (genuine coordination function between indigenous authority and institutional knowledge production, plus asymmetric extraction of institutional credibility); the naturalist reading will instantiate false-summit mountain (naturalizing what is institutional gatekeeping). The mandatrophy is resolved not by choosing one reading as 'correct' but by recognizing that the kernel itself (the anthropological record) is contested, and different readings instantiate different constraints with different ε values. The six perspectives show how indexical position determines experienced type: those with institutional power see rope or piton; those without see snare; organized actors see tangled rope; the analytical observer sees the false summit. No single type is 'the' classification — the presheaf of readings over the kernel IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    oral_tradition_fidelity_threshold,
    'Over what timescale does oral tradition cease to preserve accurate genealogical and spatial information? At what generational depth do founder myths dominate recorded fact?',
    'Cross-validation: compare oral genealogies with genetic markers, dated archaeological layers, and historical documentation for populations with known migration chronologies. Identify systematic errors and drift patterns.',
    'If fidelity >80% over 8 generations (≈200 years): oral tradition and written records have equivalent reliability constraints. If <60% beyond 6 generations: naturalist reading gains structural support. If variance is high and context-dependent: no universal threshold exists — fidelity depends on transmission mechanism and community investment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oral_tradition_fidelity_threshold, empirical, 'Temporal fidelity boundaries of oral genealogical transmission').

omega_variable(
    indigenous_authority_over_interpretation,
    'Who has legitimate authority to interpret the record — indigenous communities claiming ancestral connection, credentialed researchers with methodology, or some hybrid adjudication? What framework governs conflict resolution when readings diverge?',
    'Documentation of actual repatriation disputes, consultation agreements, and institutional policies; analysis of power distribution in joint research agreements; tracking whose interpretation is cited in published work.',
    'If indigenous authority is genuine (not advisory): constraint reclassifies toward indigenous_epistemology_reading as accurate structural model. If indigenous input is performative (tokenistic consultation): constraint reclassifies toward natural_history_extraction_snare. If hybrid authority emerges (genuine joint control): constraint becomes scaffold with negotiated sunset clause.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_authority_over_interpretation, empirical, 'Actual authority distribution in anthropological record interpretation').

omega_variable(
    epistemological_commensurability,
    'Are oral tradition and material evidence commensurable ways of knowing the past, or are they fundamentally incommensurable epistemic frameworks? Can they be integrated into a single account or only held as parallel narratives?',
    'Case studies of successful integration: where oral tradition and archaeological evidence converge or diverge, and how researchers have handled discordance. Philosophical analysis of what ''commensurability'' requires.',
    'If commensurable: indigenous_epistemology_reading accurately models a hybrid knowledge system (tangled_rope fits). If incommensurable: readings coexist but cannot be reconciled — this reading forecloses the naturalist_reading within any single unified framework (foreclosure relation). If parallel-but-unintegrable: coexists_with relation is correct, and the constraint is really about institutional jurisdiction over different knowledge domains, not about knowledge itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemological_commensurability, conceptual, 'Epistemological commensurability of oral tradition and material evidence').

omega_variable(
    kernel_reading_ambiguity,
    'Does ''the record'' refer to material objects (bones, artifacts, stratigraphy) that exist independently of interpretation, or does it include the interpretive community''s acts of recognition, curation, and narrative? Is the record a thing or a practice?',
    'Examine what ''the record'' means in practice: in repatriation cases, what are parties actually fighting over — the physical objects themselves, or authority to narrate their significance? Does handing over remains to indigenous communities constitute ''the record'' being read differently, or the record ceasing to exist for the museum?',
    'If ''record'' = material objects independent of interpretation: naturalist_reading and indigenous_epistemology_reading both apply to the same objects; commensurability question (omega above) becomes central. If ''record'' = interpretive practice: the two readings reference different records entirely (different constraints). If hybrid: the constraint itself is ontologically contested — which is what the kernel frame is designed to capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ontological status of ''the record'' itself').

omega_variable(
    false_summit_naturalism,
    'Does the naturalist claim that ''only material evidence counts'' rest on epistemological arguments (oral tradition is inherently unreliable) or on institutional gatekeeping (credentialed researchers control epistemic authority)? If institutional, the naturalist reading is extractive (snare), not a neutral natural law.',
    'Analyze naturalist literature: does it present material-evidence priority as derived from the nature of knowledge, or as enforced through institutional norms, funding, and disciplinary gatekeeping? Does it suppress countervailing evidence or alternative frameworks?',
    'If naturalism is epistemologically grounded: it is a legitimate reading coexisting with indigenous epistemology reading (coexists_with relation). If naturalism is institutionally enforced (suppression = institutional gatekeeping): this reading forecloses the false naturalist mountain, revealing it as extractive snare. The ''natural law'' framing naturalizes what is actually institutional power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturalism, empirical, 'Whether naturalist epistemology is a legitimate framework or false summit of institutional control').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__indigenous_epistemology_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_ind_ep_theater_t0, anthropological_record__indigenous_epistemology_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(anth_ind_ep_theater_t10, anthropological_record__indigenous_epistemology_reading, theater_ratio, 10, 0.54).
narrative_ontology:measurement(anth_ind_ep_theater_t20, anthropological_record__indigenous_epistemology_reading, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(anth_ind_ep_extractiveness_t0, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(anth_ind_ep_extractiveness_t10, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(anth_ind_ep_extractiveness_t20, anthropological_record__indigenous_epistemology_reading, base_extractiveness, 20, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(anth_ind_ep_suppression_t0, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(anth_ind_ep_suppression_t10, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(anth_ind_ep_suppression_t20, anthropological_record__indigenous_epistemology_reading, suppression_requirement, 20, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__indigenous_epistemology_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, anthropological_record__creationist_reading).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, repatriation_rights__indigenous_sovereignty).
narrative_ontology:affects_constraint(anthropological_record__indigenous_epistemology_reading, museum_epistemology__curatorial_authority).

% DUAL FORMULATION NOTE:
% The anthropological record kernel decomposes into three constraint stories, one per reading. Each instantiates a different constraint with different ε values and different beneficiary/victim structures. The indigenous_epistemology_reading (this story) models extraction from indigenous communities by institutional credentialing (0.48); the naturalist_reading will model false-summit suppression of alternative epistemologies; the creationist_reading will model textual authority claims. All three are linked as readings of a single contested kernel. Decomposition reflects ε-invariance: the 'record' is differently constituted under each reading, making them structurally distinct constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__indigenous_epistemology_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
