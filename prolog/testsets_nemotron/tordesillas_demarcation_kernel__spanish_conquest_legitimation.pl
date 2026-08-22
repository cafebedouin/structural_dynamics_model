% ============================================================================
% CONSTRAINT STORY: tordesillas_demarcation_kernel__spanish_conquest_legitimation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tordesillas_demarcation_kernel__spanish_conquest_legitimation, []).

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
 *   constraint_id: tordesillas_demarcation_kernel__spanish_conquest_legitimation
 *   human_readable: Papal Grant as License for Spanish Conquest West of Tordesillas Line
 *   domain: international_law/colonial_history/sovereignty_theory
 *
 * SUMMARY:
 *   The 1494 Treaty of Tordesillas, mediated by Pope Alexander VI, divided
 *   the non-European world between Spain and Portugal along a meridian 370
 *   leagues west of Cape Verde. The Spanish reading treats the papal bull
 *   Inter caetera (1493) and the treaty as a grant of dominion — not merely a
 *   European non-interference agreement — authorizing conquest, settlement,
 *   and forced conversion of indigenous peoples west of the line. This
 *   reading powered the requerimiento (legal ritual demanding submission),
 *   the encomienda system (forced labor grants), and the Patronato Real
 *   (crown control over church appointments in the Indies). The extraction is
 *   material (silver, labor, land) and spiritual (forced conversion as legal
 *   prerequisite for 'just' war). The Portuguese reading, by contrast, treats
 *   the same instruments as confirming Portugal's exploration monopoly east
 *   of the line and excluding Spanish rivals — a coordination mechanism among
 *   European powers with minimal indigenous victim structure in the
 *   Portuguese zone at this period. The two readings are distinct constraints
 *   sharing a kernel; they have different beneficiary/victim structures,
 *   different extractiveness profiles, and different temporal trajectories.
 *
 * KEY AGENTS:
 *   - spanish_colonial_administration: Primary agenda-setter (institutional/arbitrage) — writes and enforces the laws of the Indies, collects the quinto real, appoints officials
 *   - spanish_crown_treasury: Primary beneficiary (institutional/arbitrage) — receives 20% of all precious metals and tribute; the extraction flows here
 *   - encomenderos: Secondary beneficiary / local enforcer (organized/constrained) — hold grants of indigenous labor and tribute; their wealth depends on the constraint's enforcement
 *   - indigenous_populations_west_of_line: Primary victims (powerless/trapped) — subjected to forced labor, tribute, displacement, epidemic disease, and forced conversion; exit options structurally eliminated by military conquest and legal disablement
 *   - missionary_orders: Dual-positioned (organized/constrained) — genuine evangelical mission but institutionally dependent on Patronato Real; their presence legitimizes the conquest while some members (Las Casas, Montesino) become internal critics
 *   - portuguese_crown: Excluded rival (institutional/arbitrage) — holds the sibling reading; its interest is the eastern hemisphere monopoly, not the western conquest mechanism
 *   - papacy: Observer / authority ground (institutional/analytical) — issued the founding bulls; later popes (Paul III, Sublimis Deus 1537) attempted to limit the reading's excesses without retracting the grant
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88).
domain_priors:suppression_score(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.92).
domain_priors:theater_ratio(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, extractiveness, 0.88).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 0.92).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(tordesillas_demarcation_kernel__spanish_conquest_legitimation, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, snare).
narrative_ontology:human_readable(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "Papal Grant as License for Spanish Conquest West of Tordesillas Line").
narrative_ontology:topic_domain(tordesillas_demarcation_kernel__spanish_conquest_legitimation, "international_law/colonial_history/sovereignty_theory").

domain_priors:requires_active_enforcement(tordesillas_demarcation_kernel__spanish_conquest_legitimation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '377f7072-1a66-4f1e-8c4f-200688d127c6').
narrative_ontology:cs_kernel_codification('377f7072-1a66-4f1e-8c4f-200688d127c6', fixed_text).
narrative_ontology:cs_authority_grounding('377f7072-1a66-4f1e-8c4f-200688d127c6', lineage).
narrative_ontology:cs_interpretation_layer_present('377f7072-1a66-4f1e-8c4f-200688d127c6').
narrative_ontology:cs_reading_relation('377f7072-1a66-4f1e-8c4f-200688d127c6', tordesillas_demarcation_kernel__portuguese_exploration_legitimation, coexists_with).
narrative_ontology:cs_axiom('377f7072-1a66-4f1e-8c4f-200688d127c6', foundational, papal_grant_confers_full_dominion).
narrative_ontology:cs_axiom_status(papal_grant_confers_full_dominion, holdable).
narrative_ontology:cs_axiom_grounding('377f7072-1a66-4f1e-8c4f-200688d127c6', papal_grant_confers_full_dominion, conventional).
narrative_ontology:cs_axiom('377f7072-1a66-4f1e-8c4f-200688d127c6', foundational, infidel_sovereignty_extinguished_by_christian_discovery).
narrative_ontology:cs_axiom_status(infidel_sovereignty_extinguished_by_christian_discovery, overridden).
narrative_ontology:cs_axiom_grounding('377f7072-1a66-4f1e-8c4f-200688d127c6', infidel_sovereignty_extinguished_by_christian_discovery, theological).
narrative_ontology:cs_reference_frame('377f7072-1a66-4f1e-8c4f-200688d127c6', alexandrine_partition_1493).
narrative_ontology:cs_drift_state('377f7072-1a66-4f1e-8c4f-200688d127c6', post_valladolid_debate_1550, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('377f7072-1a66-4f1e-8c4f-200688d127c6', '').
narrative_ontology:cs_kernel_id(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_treasury).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos).
narrative_ontology:constraint_victim(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papal_temporal_authority_over_infidels).
narrative_ontology:constraint_vindicates(tordesillas_demarcation_kernel__spanish_conquest_legitimation, discovery_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writes and enforces the Laws of the Indies, appoints viceroys and audiencias, collects the quinto real (20% of precious metals), and administers the encomienda system. The administration's legitimacy and revenue depend entirely on the papal grant's interpretation as dominion. It could theoretically abandon the colonies (arbitrage exit) but the extraction is too valuable; the constraint is maintained by choice, not necessity.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_colonial_administration, agenda_setter,
    institutional, generational, arbitrage, continental).

% Receives the quinto real and other colonial revenues — ~180 tons of silver 1503-1660, plus customs duties and tribute. The treasury is the ultimate sink of the constraint's extraction. It has arbitrage-grade exit (could renounce claims) but the revenue stream is foundational to Habsburg and Bourbon fiscal-military power.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, spanish_crown_treasury, beneficiary,
    institutional, generational, arbitrage, continental).

% Hold grants of indigenous labor and tribute in exchange for 'protection' and Christianization. They extract locally but depend on crown enforcement to maintain grants; they pay taxes and military service to the crown. Their exit is constrained: grants are non-transferable, revocable, and tied to residence; they cannot easily liquidate and leave.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomenderos, payer).

% Subjected to forced labor (encomienda, mita, repartimiento), tribute payments in kind and specie, land dispossession, forced relocation (reducciones), and forced conversion. Legal personhood denied until 1537 (Sublimis Deus), and the bull was ignored in practice. No exit: military conquest destroyed independent polities; disease demolished demographic capacity to resist; Spanish law forbade indigenous departure from assigned pueblos. Identity-locked: the conquest fused their political existence to Spanish sovereignty — 'subjects of the king' replaced 'nations' as their legal status.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, indigenous_populations_west_of_line, payer,
    powerless, generational, trapped, continental).

% Franciscans, Dominicans, Jesuits, Augustinians: run missions, schools, hospitals; produce indigenous-language grammars and catechisms; some (Las Casas, Montesino, Sahagún) document abuses and advocate reform. They invest real resources and personnel — genuine evangelical effort. But they operate under Patronato Real: crown appoints bishops, funds missions, controls entry. Their institutional position depends on the constraint; their critiques are internal, not structural. Exit is constrained: leaving means abandoning converts and infrastructure; the crown can expel orders (Jesuits 1767).
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(tordesillas_demarcation_kernel__spanish_conquest_legitimation, missionary_orders, payer).

% Holds the sibling reading (portuguese_exploration_legitimation). Its interest is the eastern hemisphere monopoly confirmed by the same kernel. It is excluded from the Spanish conquest mechanism — not a victim of it, but a rival claimant to the kernel's authority. The Portuguese reading's beneficiary is the Portuguese crown; its victim set at this period is minimal (Brazilian indigenous subjugation intensifies later, under a different constraint story).
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, portuguese_crown, excluded,
    institutional, generational, arbitrage, continental).

% Issued the founding bulls (Inter caetera, Eximiae devotionis). Later popes attempted to limit excesses: Paul III's Sublimis Deus (1537) affirmed indigenous rationality and banned enslavement; Urban VIII's Commissum nobis (1639) condemned slavery. But the papacy never retracted the dominion grant, and the Patronato Real gave the Spanish crown control over church appointments in the Indies — the papacy became a captive legitimator. Its seat is analytical: it sees the full structure but cannot change it without undermining its own historical authority.
narrative_ontology:constraint_stakeholder(tordesillas_demarcation_kernel__spanish_conquest_legitimation, papacy, observer,
    institutional, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Mediated the Iberian rivalry over Atlantic claims to prevent European war; provided a legal framework for Spanish governance of vast territories (the Laws of the Indies); established a nominally universal Christian order in the Americas replacing indigenous political forms.
% TRANSFER_FUNCTION: Moves land, labor, and precious metals from indigenous populations to Spanish crown treasury (quinto real), encomenderos (tribute/labor), and missionary orders (indigenous labor for mission infrastructure). Moves spiritual authority from indigenous cosmologies to Catholic Church (forced conversion as legal prerequisite). Moves political sovereignty from indigenous polities to Spanish crown (requerimiento ritual).
% ABSENT_VOICES: Indigenous political authorities (tlatoani, Inca, caciques) were physically present at first contact but structurally excluded from the treaty negotiation and the legal framework it authorized. Their objections were recorded only in Spanish transcripts (requerimiento responses, later legal petitions). African populations (later brought as slaves) were never consulted. The Portuguese crown was a party to the treaty but not to the Spanish conquest reading's victim structure.
% DISAPPEARANCE_RATIONALE: If the papal grant's interpretation as conquest license vanished overnight: the legal basis for requerimiento, encomienda, and Patronato Real collapses; indigenous polities could claim legal sovereignty under natural law (Vitoria's arguments); the Spanish crown would lose its primary legitimating title; the silver fleet system would lose its legal foundation; the entire colonial administrative structure would require reconstruction on a new basis (conquest by force alone, or negotiated treaties). The world rearranges because the constraint is the keystone of the Spanish imperial legal order in the Americas.
% FOUNDING_PROBLEM: Prevent war between Spain and Portugal over competing Atlantic discoveries; provide papal mediation that both Catholic monarchs would accept; replace Portuguese-sponsored exploration monopoly with a bilateral division recognized by the highest Christian authority.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (Iberian rivalry mediation) was resolved by 1529 Treaty of Zaragoza, which completed the global division. The Spanish crown's own historians (Herrera, Solís) and the Portuguese chronicles (Barros, Correa) attest the rivalry was settled. The constraint persisted 300 years after — the Council of the Indies, the Laws of the Indies, the Patronato Real, the silver fleet system all operated long after the Iberian war risk vanished. No non-beneficiary source attests the founding problem remained live; the continuation is attested only by the extraction machinery's beneficiaries.
narrative_ontology:disappearance_verdict(tordesillas_demarcation_kernel__spanish_conquest_legitimation, world_rearranges).
narrative_ontology:founding_problem_status(tordesillas_demarcation_kernel__spanish_conquest_legitimation, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 'none', 1).
narrative_ontology:epsilon_provenance(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.88, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tordesillas_demarcation_kernel__spanish_conquest_legitimation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tordesillas_demarcation_kernel__spanish_conquest_legitimation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.88) reflects the scale of material transfer: the quinto real alone moved ~180 tons of silver to Seville 1503-1660; encomienda tribute in kind and labor was the primary extraction mechanism for the first century. Suppression (0.92) is near-maximum: the requerimiento ritualized surrender; refusal triggered 'just war' enslavement; indigenous legal personhood was denied until the 1537 bull (which was ignored in practice); the Laws of the Indies (1542, 1573) regulated but did not dismantle the extraction. Theater ratio (0.25) captures the genuine but subordinated evangelical effort: missions were real and costly, but their legal function was to certify conversion as a condition for 'peaceful' incorporation — the conversion certificate was the administrative gateway to encomienda assignment. Accessibility collapse (0.85): alternatives (indigenous sovereignty, trade relations, voluntary conversion) were militarily and legally foreclosed. Resistance (0.65) is substantial but fragmented: major rebellions (Mixtón, Chichimeca, Pueblo Revolt, Tupac Amaru) and persistent legal resistance in Spanish courts, but demographic collapse (90%+ mortality in many regions) and military asymmetry prevented coordinated overthrow.
 *
 * PERSPECTIVAL GAP:
 *   From the Spanish administrative seat, the constraint appears as a tangled_rope: genuine coordination problems (governing vast territories, integrating diverse populations, preventing Portuguese incursion) solved by a system that extracts heavily but provides administrative order. From the indigenous seat, it is a pure snare: the coordination story is cover; the system's persistence depends on suppressing indigenous political forms and extracting labor/land. The missionary seat experiences the constraint as a scaffold: a transitional structure meant to produce Christian societies that would eventually be self-governing (the 'republic of Indians' ideal) — but the sunset never arrived. The engine computes these seat divergences from the structural data; the single claimed_type (snare) represents the analytical observer's assessment of the constraint's dominant structural character.
 *
 * DIRECTIONALITY LOGIC:
 *   The Spanish Crown and colonial administration are full beneficiaries (d ≈ 0.05): they collect extraction, write the rules, hold arbitrage-grade exit (could abandon colonies but chose not to). Encomenderos are partial beneficiaries (d ≈ 0.25): they extract locally but depend on crown enforcement; their exit is constrained (land grants non-transferable, revocable). Missionary orders sit near symmetric (d ≈ 0.5): they invest real resources in conversion but gain institutional position and spiritual authority. Indigenous populations are full targets (d ≈ 0.95): identity-locked (conquest fuses their political existence to Spanish sovereignty), trapped (no exit from the spatial scope of the constraint), and the primary source of extracted value. Portuguese Crown is excluded from this constraint's beneficiary/victim structure — its reading operates on a different spatial scope (east of line) with a different victim set (minimal at this period).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (papal mediation of Iberian rivalry to prevent European war over Atlantic claims) was substantially resolved by 1529 (Treaty of Zaragoza). The constraint persisted for 300 years after its founding problem died, maintained by the extraction machinery it built (encomienda, silver fleets, colonial bureaucracy). This is mandatrophy in its purest form: the arrangement outlived its justification by centuries, and the extraction became the function. The mandate was not resolved — it was inverted: the papal grant became the license for an extraction system that the papacy itself later condemned but could not dismantle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the Spanish conquest legitimation reading a distinct constraint from the Portuguese exploration legitimation reading, or two angles on the same structural mechanism?',
    'Compare beneficiary/victim structures and extractiveness profiles: Spanish reading has indigenous populations as primary victims and encomienda as extraction mechanism; Portuguese reading has rival European powers as excluded parties and exploration monopoly as benefit. Different ε referents (standing arrangements under contest) confirm distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification (snare vs. rope/tangled_rope). If same, a single constraint story must accommodate both victim sets — unlikely given divergent extraction mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment-system kernel decomposition: this constraint is the spanish_conquest_legitimation reading of the tordesillas_demarcation_kernel; the portuguese_exploration_legitimation reading is a sibling constraint.').

omega_variable(
    indigenous_resistance_agency,
    'Does the measured resistance (0.65) adequately capture organized indigenous military, legal, and cultural resistance, or does the suppression apparatus obscure its scale?',
    'Historiographic recovery of indigenous archival sources, archaeological evidence of sustained rebellions, and legal petitions to Spanish courts (e.g., Valladolid debate, requerimiento refusals).',
    'If resistance is systematically undercounted, the constraint''s snare classification is reinforced — high suppression meeting higher-than-recorded resistance is the snare signature. If resistance was genuinely low after initial contact, the constraint''s persistence mechanism shifts toward demographic collapse rather than active suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_resistance_agency, empirical, 'Resistance measurement in colonial contexts where the suppressed population''s records were destroyed or never created.').

omega_variable(
    theater_of_christianization,
    'Is the theater_ratio (0.25) an accurate reflection of genuine evangelical effort vs. extraction cover, or does the theological frame genuinely motivate institutional behavior beyond rent-seeking?',
    'Compare resource flows: missionary staffing and funding vs. encomienda revenue extraction; track institutional persistence of missions after economic value extraction exhausted.',
    'If evangelical effort was substantial and independent of extraction, theater_ratio should be lower and the constraint edges toward tangled_rope (genuine coordination function: religious integration). If conversion was primarily a legal prerequisite for encomienda legitimacy, theater_ratio is higher and snare classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_of_christianization, conceptual, 'Whether the religious justification is a coordination function or extraction cover.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 1494, 1820).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1494, 0.15).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1520, 0.2).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1570, 0.25).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1650, 0.28).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1750, 0.3).
narrative_ontology:measurement(tordesillas_spanish_conquest_tr_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, theater_ratio, 1820, 0.25).

% Extraction over time
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1494, 0.35).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1520, 0.65).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1570, 0.82).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1650, 0.88).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1750, 0.9).
narrative_ontology:measurement(tordesillas_spanish_conquest_be_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, base_extractiveness, 1820, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1494, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1494, 0.7).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1520, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1520, 0.85).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1570, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1570, 0.92).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1650, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1650, 0.93).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1750, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1750, 0.94).
narrative_ontology:measurement(tordesillas_spanish_conquest_su_t1820, tordesillas_demarcation_kernel__spanish_conquest_legitimation, suppression_requirement, 1820, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tordesillas_demarcation_kernel__spanish_conquest_legitimation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, 0.12).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, tordesillas_demarcation_kernel__portuguese_exploration_legitimation).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, requerimiento_legal_ritual).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, encomienda_system).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, patronato_real).
narrative_ontology:affects_constraint(tordesillas_demarcation_kernel__spanish_conquest_legitimation, laws_of_the_indies).

% DUAL FORMULATION NOTE:
% This constraint and portuguese_exploration_legitimation are sibling readings of the tordesillas_demarcation_kernel. The Spanish reading instantiates a high-extraction snare targeting indigenous populations; the Portuguese reading instantiates a lower-extraction coordination mechanism among European powers (at least in its initial period). The ε values differ by >0.5, confirming they are distinct constraints per ε-invariance. They are linked via affects_constraints because the Spanish conquest legitimation reading cites the Portuguese exclusion as partial justification for its own western monopoly, and the Portuguese reading's eastern monopoly was the quid pro quo for Spanish western claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, institutional, 0.05).
constraint_indexing:directionality_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, organized, 0.25).
constraint_indexing:directionality_override(tordesillas_demarcation_kernel__spanish_conquest_legitimation, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
