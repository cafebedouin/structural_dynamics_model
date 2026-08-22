% ============================================================================
% CONSTRAINT STORY: maat_order_principle__divine_mandate_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__divine_mandate_reading, []).

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
 *   constraint_id: maat_order_principle__divine_mandate_reading
 *   human_readable: Ma'at Divine Mandate — Pharaoh as Source and Embodiment
 *   domain: political_philosophy/religious_studies/ancient_history
 *
 * SUMMARY:
 *   The divine-mandate reading of Ma'at positions the pharaoh as the
 *   exclusive conduit through which cosmic order flows to the material world.
 *   In this reading, the pharaoh EMBODIES Ma'at and cannot violate it by
 *   definition — pharaonic action, by virtue of the pharaoh's divine status,
 *   is necessarily Ma'at-compliant. This reading removes the pharaoh from any
 *   constraint system that could bind or limit royal action. Extraction is
 *   justified as cosmic necessity: taxes and labor are not imposed by the
 *   pharaoh but flow from the pharaoh as Ma'at incarnate. Resistance or
 *   complaint is reframed as cosmic disorder. This reading is one of three
 *   contested interpretations of the Ma'at kernel — it coexists with the
 *   reciprocity reading (which holds the pharaoh must provide justice to
 *   maintain balance) and the distributed-maintenance reading (which
 *   distributes Ma'at responsibility across all social levels). The
 *   divine-mandate reading's persistence depends on suppressing the other
 *   readings' interpretive authority.
 *
 * KEY AGENTS:
 *   - Pharaoh: institutional actor positioned as cosmic conduit and source of Ma'at; extraction justified by definition; exit = analytical (cosmological role is non-negotiable)
 *   - Subject population: powerless, trapped; bear corvée, taxation, and obedience justified as cosmic law; no legitimate frame for resistance
 *   - Competing priesthoods: organized actors whose local Ma'at authority is subordinated to pharaonic interpretation; constrained exit (lose position if they dissent)
 *   - Regional administrators: organized, constrained; share extraction benefit but cannot challenge pharaonic authority without losing legitimacy
 *   - Theoretical dissenters: excluded from conversation; cannot write, preach, or organize alternative readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, 0.82).
domain_priors:suppression_score(maat_order_principle__divine_mandate_reading, 0.91).
domain_priors:theater_ratio(maat_order_principle__divine_mandate_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(maat_order_principle__divine_mandate_reading, resistance, 0.34).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__divine_mandate_reading, snare).
narrative_ontology:human_readable(maat_order_principle__divine_mandate_reading, "Ma'at Divine Mandate — Pharaoh as Source and Embodiment").
narrative_ontology:topic_domain(maat_order_principle__divine_mandate_reading, "political_philosophy/religious_studies/ancient_history").

domain_priors:requires_active_enforcement(maat_order_principle__divine_mandate_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__divine_mandate_reading, '6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56').
narrative_ontology:cs_kernel_codification('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', fixed_text).
narrative_ontology:cs_authority_grounding('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', extraction).
narrative_ontology:cs_interpretation_layer_present('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56').
narrative_ontology:cs_reading_relation('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', maat_order_principle__reciprocity_reading, forecloses).
narrative_ontology:cs_reading_relation('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', foundational, pharaoh_embodies_maat_absolute).
narrative_ontology:cs_axiom_status(pharaoh_embodies_maat_absolute, holdable).
narrative_ontology:cs_axiom_grounding('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', pharaoh_embodies_maat_absolute, theological).
narrative_ontology:cs_axiom('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', foundational, pharaonic_action_cannot_violate_cosmic_order).
narrative_ontology:cs_axiom_status(pharaonic_action_cannot_violate_cosmic_order, holdable).
narrative_ontology:cs_axiom_grounding('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', pharaonic_action_cannot_violate_cosmic_order, deontological).
narrative_ontology:cs_reference_frame('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', pharaonic_cosmic_embodiment).
narrative_ontology:cs_drift_state('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', late_period_pharaonic_decline, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6e14fd04-6fe2-4057-9e7a-fe4d74e2ff56', '').
narrative_ontology:cs_kernel_id(maat_order_principle__divine_mandate_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, subject_population).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, competing_priesthoods).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, regional_administrators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(maat_order_principle__divine_mandate_reading, merchant_and_scribal_class).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, merchant_and_scribal_class).
narrative_ontology:constraint_victim(maat_order_principle__divine_mandate_reading, foreign_captives_and_slaves).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Positioned as the embodiment and sole legitimate conduit of Ma'at — cosmic order flows through the pharaoh's person to maintain both natural and social order. All pharaonic action is defined as Ma'at-compliant by virtue of the pharaoh's divine status. Extracts labor, resources, and absolute obedience justified as cosmic necessity. Cannot violate Ma'at by definition, since pharaonic action constitutes Ma'at's expression in the material world.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, pharaoh, agenda_setter,
    institutional, generational, analytical, national).

% Bears the material cost of pharaonic governance: taxation, corvée labor for state projects, conscription, and absolute obedience. Told their cooperation maintains cosmic order and their station is divinely ordained. Resistance or complaint is framed as cosmic disorder. No legitimate exit: the cosmological claim is that they are embedded in an order they did not choose and cannot challenge without inviting supernatural chaos.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, subject_population, payer,
    powerless, biographical, trapped, national).

% Local and regional priesthoods originally maintained their own Ma'at authority through ritual and moral interpretation. The divine mandate reading subordinates all priesthoods to pharaonic interpretation of cosmic order. They retain ceremonial roles but lose interpretive autonomy. Their resistance is suppressed through theological subordination (pharaoh is the high priest; other priests are functionaries executing pharaonic will) and through control of temple resources and appointments.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, competing_priesthoods, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, competing_priesthoods, observer).

% Exercise delegated authority but hold it precariously, contingent on pharaonic will. They extract locally in the pharaoh's name, justified by the same cosmic-order framing. They share extraction benefit with the central authority but cannot challenge pharaonic interpretation of Ma'at without losing legitimacy and position. Their authority is borrowed, not earned; it evaporates if they question the source.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, regional_administrators, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, regional_administrators, agenda_setter).

% Benefit from the stability and unified legal/commercial framework the pharaonic order provides. Also bear taxation and contingent service obligations. They have more exit than the subject population (skill portability, trade relationships) but are still trapped within the Egyptian economy and subject to pharaonic confiscation or conscription. Their benefit is real but contingent on compliance.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, merchant_and_scribal_class, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(maat_order_principle__divine_mandate_reading, merchant_and_scribal_class, payer).

% The lowest tier of the extraction apparatus: taken in war or trade, worked to death on monuments and in mines. The divine mandate reading offers them no status and no claim on Ma'at's protection — they are treated as outside the cosmic order entirely, chattels without personhood. Their situation exemplifies the reading's suppressive capacity: no cosmological appeal possible, no alternative frame available.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, foreign_captives_and_slaves, payer,
    powerless, immediate, trapped, national).

% Intellectuals, priests, or administrators who privately or quietly advocate for the reciprocity or distributed-maintenance readings of Ma'at are structurally excluded from the conversation. They cannot write their objections (royal scribal monopoly); cannot preach their interpretations (priestly hierarchy subordinated); cannot organize resistance (surveillance and suppression apparatus enforcing the divine-mandate reading). Their absence from the official record is itself suppression.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, theoretical_dissenting_voices, excluded,
    powerless, biographical, trapped, national).

% Examines the constraint's operation from outside Egyptian cosmology. Can see that the divine-mandate reading concentrates all legitimacy and interpretive authority in a single office, places extraction beyond challenge by cosmological fiat, and suppresses competing readings through theological monopoly. Records how the reading's persistence depends on enforcing belief in the pharaoh's cosmic status.
narrative_ontology:constraint_stakeholder(maat_order_principle__divine_mandate_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__divine_mandate_reading, pharaoh).
narrative_ontology:fixing_cost_class(maat_order_principle__divine_mandate_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified cosmological framework justifying centralized order: a single source of authority (pharaoh as Ma'at embodied) eliminates negotiation and coordination costs among competing power centers. All actors know their place and obligations derive from cosmic law, not negotiation. The coordination solves the problem of how to govern without explaining.
% TRANSFER_FUNCTION: Moves resources (grain, labor, raw materials, tribute), obedience, and interpretive authority upward to the pharaoh and the central bureaucracy. Moves legitimacy, security (against external enemies and internal disorder), and divine favor (in theory) downward, though the distribution of these goods is entirely controlled by the pharaoh. The actual net transfer is overwhelmingly upward: material extraction for subjects, immaterial (cosmological justification) for administrators.
% ABSENT_VOICES: Priests and scribes who maintain reciprocity or distributed-maintenance readings of Ma'at are structurally excluded from the conversation. They cannot publicly dispute pharaonic interpretation without committing theological heresy. Commoner intellectuals, foreign slaves, and women (who have no official voice in the theological discourse despite bearing extraction) are locked out entirely. Their absence is maintained through suppression of literacy, exile of dissenting priests, and destruction of alternative texts.
% DISAPPEARANCE_RATIONALE: If the divine-mandate reading of Ma'at vanished overnight, the pharaonic extraction apparatus would lose its cosmological justification. Without the claim that the pharaoh embodies cosmic order and cannot violate Ma'at, resource extraction requires explicit coercion or negotiated tribute instead of theological compliance. Competing priesthoods would resurrect the reciprocity reading (Pharaoh must provide justice to maintain balance); regional administrators would face pressure to justify their authority locally rather than delegating to the pharaoh. The unified bureaucratic state depends on the reading's suppression of alternative frames.
% FOUNDING_PROBLEM: Pre-state Egypt faced fragmented authority among competing regional powers, priesthoods, and noble families. The divine-mandate reading solved the coordination problem of unifying these powers under a single source of legitimacy that could not be negotiated or challenged: if the pharaoh IS Ma'at, then obedience to the pharaoh is obedience to cosmic law itself. This reading justified the consolidation of Egypt under one ruler.
% FOUNDING_PROBLEM_CORROBORATION: The pharaonic bureaucracy and the temple establishment (in its subordinate form) attest the founding problem persists: internal order still requires unified authority against regional fragmentation. Priests who maintain the reciprocity reading (attested in later Wisdom Literature and some priestly texts) dispute this: they argue the problem is not fragmentation but the pharaoh's abuse of power, which a genuinely reciprocal Ma'at would constrain. Modern Egyptologists divided on the claim: some read New Kingdom inscriptions as confirming perpetual divine mandate; others read the same sources as showing persistent tension between the reading and actual pharaonic behavior (documented violations of Ma'at norms, succession disputes, collapse during interregna). No external corroboration exists — this is a claim internal to ancient Egyptian cosmology.
narrative_ontology:disappearance_verdict(maat_order_principle__divine_mandate_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__divine_mandate_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__divine_mandate_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(maat_order_principle__divine_mandate_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__divine_mandate_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__divine_mandate_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__divine_mandate_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__divine_mandate_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.68 → 0.82) because the consolidation of pharaonic authority progressively eliminates local power centers that might negotiate or resist. Suppression rises sharply (0.74 → 0.91) because the divine-mandate reading requires active enforcement of a cosmological monopoly: priests and scribes who maintain alternative readings must be silenced, texts must be controlled, dissent must be rendered literally unthinkable (positioned as cosmic heresy). Theater rises moderately (0.52 → 0.68) because the constraint's persistence increasingly depends on ceremonial performance — monuments, rituals, inscriptions all assert pharaonic divinity — as actual pharaonic behavior occasionally deviates from Ma'at norms (documented violations of justice, arbitrary confiscation, succession instability). The machinery of assertion grows as evidence of actual Ma'at-compliance decreases. Accessibility collapse is high (0.79) because the reading's cosmological claim is absolute: there is no frame in which the pharaoh is subject to Ma'at constraint. Resistance is low (0.34) because the apparatus of suppression is effective; dissent is pushed underground or silenced entirely. The payer and agenda-setter seats compute entirely differently: the pharaoh experiences the constraint as enabling (no limits, no accountability); the subject population experiences it as total (no exit, no appeal).
 *
 * PERSPECTIVAL GAP:
 *   The pharaoh's position: the divine-mandate reading positions the pharaoh OUTSIDE any constraint system, as the source of legitimacy. From this seat, Ma'at is not a constraint but a description of pharaonic action — the pharaoh cannot violate Ma'at because pharaonic action IS Ma'at. The pharaoh experiences this as freedom and cosmic authority. The payer seats (subject population, priesthoods, administrators): the reading constrains them absolutely — their role is to obey, sustain, and never question. From these seats, the reading is a snare: it removes all leverage and legitimate grounds for resistance by placing the pharaoh outside the system of mutual obligations. The regional administrator seat shows internal divergence: they benefit from the delegation of extraction authority (share the gains, enjoy higher status than subjects) but are structurally vulnerable (their authority is borrowed, revocable). The engine computes these divergences as different directionalities for the same constraint: beneficiary-adjacent d for the pharaoh (extraction without cost), target-adjacent d for powerless subjects (maximum extraction, no exit), and intermediate d for administrators (extraction and benefit both, but threatened).
 *
 * DIRECTIONALITY LOGIC:
 *   The pharaoh is a structural beneficiary (collects all extraction, faces no accountability) but positioned at d = analytical because the reading removes the pharaoh from the constraint entirely — the pharaoh is the source, not a seat subject to constraint. This is captured via override: the pharaoh's power level should derive to high d (target-like), but the reading's structure places the pharaoh outside, so d = 0.0 (full beneficiary, more precisely: exempt). Subject population and competing priesthoods are full targets: high d (0.95+), trapped exit, powerless position, bearing extraction. Regional administrators are intermediate targets (d = 0.60–0.75): they extract locally but cannot challenge centrally, benefiting from delegation while constrained by revocable authority. Merchants and skilled classes are mild targets (d = 0.40–0.55): constrained, taxed, but more exit than subjects and some benefit from order. The reading's structure is deliberately asymmetric: the pharaoh's d-value would be computed as 0.0 (exempt/beneficiary) without override; with override acknowledging the cosmic-source framing, it remains 0.0 but with the semantic shift from beneficiary-with-costs to source-without-system. This is the structural novelty of the divine-mandate reading: it removes the top from the system entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a high risk of mandatrophy resolution (founding problem dead, constraint persists). The founding problem was political consolidation — unifying fragmented regional powers under a single authority. By the New Kingdom, regional fragmentation is solved; pharaonic authority is established. But the divine-mandate reading persists, now operating as pure extraction justified by cosmology rather than by coordination need. The constraint's persistence depends on theatrical assertion (rising theater_ratio) precisely because the original coordination problem is solved. The rising suppression_requirement signals that the reading's cosmological claim is contested — dissenters exist and must be silenced, suggesting the founding-problem resolution has created space for the alternative readings to re-emerge. A mandatrophy reading would argue: 'The divine-mandate reading solved real coordination problems in the state's formation, but now operates as a snare justifying extraction without coordination function. The suppression of reciprocity and distributed-maintenance readings is what keeps the constraint alive, not the reading's truth or necessity.' This analysis would classify the constraint as a piton (atrophied function, theatrical maintenance, no party benefits enough to justify the cost except the pharaoh, who captures the extraction). The engine should flag the founding_problem_status=contested + disappearance_verdict=world_rearranges mismatch as a mandatrophy candidate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cosmological_claim_vs_institutional_fact,
    'Is the divine-mandate reading a descriptive account of how Ma''at actually flows through pharaonic authority, or a normative justification for extraction that uses cosmological language to preclude challenge?',
    'Examine historical records of pharaonic violations of Ma''at norms (documented cases of unjust confiscation, succession disputes, failures to provide stability) alongside priestly and scribal responses: were violations treated as cosmic disorder requiring explanation, or as local/temporary failures not affecting the overall cosmological claim? If the latter, the reading functions as normative justification (postcondition always satisfied); if the former, it claims to describe cosmological reality (postcondition can fail).',
    'If the reading is normative justification, it is a snare: extraction is justified by cosmological fiat that can never be falsified. If it is descriptive, it is constrained by empirical reality — actual pharaonic failure to maintain Ma''at would undermine the reading''s authority. This distinction maps to the snare vs. tangled-rope boundary: a snare offers no legitimate frame for resistance; a tangled rope offers constraint that could, in principle, be enforced if the pharaoh violated it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cosmological_claim_vs_institutional_fact, conceptual, 'Whether divine-mandate is a factual claim about cosmic order or a rhetorical justification for exemption from constraint.').

omega_variable(
    suppression_mechanism_internalized,
    'Is the suppression of the reciprocity and distributed-maintenance readings maintained by external force (exile, execution of dissenters, textual destruction) or by internalized acceptance of the divine-mandate frame as cosmological truth?',
    'Compare the suppression intensity across populations: does it vary with proximity to power (more intense suppression of priestly and scribal dissent near the palace, less intense in remote regions)? In later periods when pharaonic power declined, did alternative readings re-emerge spontaneously, suggesting suppression was structural rather than internalized? Did scribal education training systematize the divine-mandate reading as the only legitimate interpretation?',
    'If suppression is primarily external, the constraint persists by force; if internalized, it persists by belief. Internalized suppression is higher-confidence (the target carries the constraint with them), but also more fragile — if the frame breaks, the population no longer contains itself. External suppression is lower-confidence (requires ongoing enforcement machinery) but more stable (independent of belief). This maps to the suppression metric: high suppression with low internalization is expensive to maintain; high suppression with high internalization is cheap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized, empirical, 'Whether the suppression of dissenting readings is structural/external or internalized/cognitive.').

omega_variable(
    pharaonic_divinity_as_genuine_cosmological_claim,
    'Did the pharaonic priesthood and educated classes genuinely believe the pharaoh was a god incarnate and Ma''at''s embodiment, or did they understand this as a useful fiction for governance?',
    'Examine private scribal correspondence, priestly discussions in non-public contexts (if available), succession disputes, and moments of pharaonic failure: do these reveal doubt about the divine-mandate claim, or consistent assertion? Do periods of regency or co-rule (when the divine-mandate reading would be doubly strained) show crisis language suggesting the cosmology was believed, or pragmatic adjustment suggesting it was strategic?',
    'If genuinely believed, the divine-mandate reading is a case of a sincere cosmological mistake that persists because it solves coordination problems and those who benefit from it have power to enforce it. If understood as useful fiction, it is a conscious snare — a false cosmological claim deployed to justify extraction. The impact on classification is minor (it is a snare either way) but the impact on how to resolve it is major: genuine belief requires changing the cosmological frame; conscious fiction requires changing the political settlement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pharaonic_divinity_as_genuine_cosmological_claim, empirical, 'Whether the divine-mandate claim was sincere religious belief or strategic fiction.').

omega_variable(
    kernel_reading_contestation_channel,
    'If the divine-mandate, reciprocity, and distributed-maintenance readings all claim to interpret the same textual and cosmological heritage, what structural factors determine which reading dominates at any given moment?',
    'Track which reading prevails during periods of strong pharaonic power (divine-mandate) vs. weak pharaonic power (reciprocity/distributed-maintenance). Correlate with textual production: which readings are transcribed and propagated in official texts vs. suppressed? Do priesthoods shift their public reading while maintaining private objections?',
    'If the reading''s dominance tracks pharaonic power rather than its truth-value, the reading is a creature of political settlement rather than of cosmological fact. This would support the mandatrophy analysis: the divine-mandate reading persists not because it is true but because those in power benefit from it and can suppress alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation_channel, empirical, 'Structural factors determining which reading of the Ma''at kernel dominates at different historical moments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__divine_mandate_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__divine_mandate_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(maat_tr_t8, maat_order_principle__divine_mandate_reading, theater_ratio, 8, 0.55).
narrative_ontology:measurement(maat_tr_t16, maat_order_principle__divine_mandate_reading, theater_ratio, 16, 0.59).
narrative_ontology:measurement(maat_tr_t24, maat_order_principle__divine_mandate_reading, theater_ratio, 24, 0.63).
narrative_ontology:measurement(maat_tr_t32, maat_order_principle__divine_mandate_reading, theater_ratio, 32, 0.66).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__divine_mandate_reading, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__divine_mandate_reading, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(maat_be_t8, maat_order_principle__divine_mandate_reading, base_extractiveness, 8, 0.72).
narrative_ontology:measurement(maat_be_t16, maat_order_principle__divine_mandate_reading, base_extractiveness, 16, 0.76).
narrative_ontology:measurement(maat_be_t24, maat_order_principle__divine_mandate_reading, base_extractiveness, 24, 0.79).
narrative_ontology:measurement(maat_be_t32, maat_order_principle__divine_mandate_reading, base_extractiveness, 32, 0.81).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__divine_mandate_reading, base_extractiveness, 40, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__divine_mandate_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement(maat_su_t8, maat_order_principle__divine_mandate_reading, suppression_requirement, 8, 0.79).
narrative_ontology:measurement(maat_su_t16, maat_order_principle__divine_mandate_reading, suppression_requirement, 16, 0.83).
narrative_ontology:measurement(maat_su_t24, maat_order_principle__divine_mandate_reading, suppression_requirement, 24, 0.87).
narrative_ontology:measurement(maat_su_t32, maat_order_principle__divine_mandate_reading, suppression_requirement, 32, 0.89).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__divine_mandate_reading, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__divine_mandate_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(maat_order_principle__divine_mandate_reading, 0.25).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__reciprocity_reading).
narrative_ontology:affects_constraint(maat_order_principle__divine_mandate_reading, maat_order_principle__distributed_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested maat_order_principle kernel. Three structurally distinct constraint stories instantiate the three competing readings: (1) divine_mandate_reading — Ma'at flows through the pharaoh alone, placing the pharaoh outside constraint; (2) reciprocity_reading — Ma'at imposes mutual obligations, constraining the pharaoh's actions; (3) distributed_maintenance_reading — Ma'at responsibility is distributed across all social levels. The ε values differ substantially across readings: the divine-mandate reading has high extractiveness (pharaoh is exempt) and high suppression (alternative readings must be silenced); the reciprocity reading has lower extractiveness (constrained by obligation to provide justice) and lower suppression (the constraint is self-reinforcing). The readings are linked via network.affects_constraints because the divine-mandate reading's suppression apparatus directly targets the reciprocity and distributed-maintenance readings' normative space — establishing the pharaoh as Ma'at's sole conduit undermines the claim that Ma'at imposes obligations on the pharaoh or is distributed universally. Each story carries its own ε-invariant analysis; this story does not average or hedge across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maat_order_principle__divine_mandate_reading, institutional, 0.0).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
