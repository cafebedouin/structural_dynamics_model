% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta 1215: Baronial Privilege Reading
 *   domain: constitutional_law/political_theory
 *
 * SUMMARY:
 *   Magna Carta of 1215 is read here as a feudal contract between King John
 *   and the landowning barons of England. The charter secures the barons
 *   against arbitrary feudal incidents — wardship abuse, merchet, relief
 *   exaction — that John used to generate revenue and punish baronial
 *   independence. The 'free men' protected by Clause 39 are exclusively the
 *   propertied class whose legal standing depended on feudal landholding. The
 *   charter's silence on commoners, villeins, and women constitutes absolute
 *   exclusion: they are not parties to the contract and receive no
 *   protection. This reading instantiates one interpretation of the Magna
 *   Carta kernel; sibling readings (universal rights, living document)
 *   construct different constraints from the same text by reframing who 'free
 *   men' includes and whether the charter's meaning evolves. The author's
 *   reading is structurally narrow: beneficiaries are the barons, victims are
 *   the crown (revenue-constrained) and the excluded populace (unprotected).
 *   The charter is not a mountain of natural law — it is an extractive
 *   institutional arrangement dressed in the language of feudal contract.
 *
 * KEY AGENTS:
 *   - landowning_barons: Primary beneficiaries; coordinate to enforce the charter against arbitrary incidents (power: powerful, exit: constrained by mutual obligation and land tenure)
 *   - crown (King John and successors): Bound payer; commits to refrain from feudal revenue extraction but loses a revenue stream and gains political legitimacy risk if violated (power: institutional, exit: trapped by oath and excommunication threat)
 *   - non_landholding_populace: Structurally excluded; charter's protections do not extend to them, no coordination function for them (power: powerless, exit: trapped)
 *   - ecclesiastical_authority: Enforcer and legitimacy granter; provides excommunication as sanction and frames the charter as a feudal-ecclesiastical obligation (power: institutional, exit: analytical)
 *   - rebel_barons: Military enforcers of the original bargain; their threat of force makes the charter credible, but they are displaced by later institutional mechanisms (power: powerful, exit: mobile to rebellion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.62).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.71).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta 1215: Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '9724925c-1d69-4ae3-8261-709692118744').
narrative_ontology:cs_kernel_codification('9724925c-1d69-4ae3-8261-709692118744', fixed_text).
narrative_ontology:cs_authority_grounding('9724925c-1d69-4ae3-8261-709692118744', lineage).
narrative_ontology:cs_interpretation_layer_present('9724925c-1d69-4ae3-8261-709692118744').
narrative_ontology:cs_reading_relation('9724925c-1d69-4ae3-8261-709692118744', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('9724925c-1d69-4ae3-8261-709692118744', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('9724925c-1d69-4ae3-8261-709692118744', foundational, free_men_feudal_property_status).
narrative_ontology:cs_axiom_status(free_men_feudal_property_status, holdable).
narrative_ontology:cs_axiom_grounding('9724925c-1d69-4ae3-8261-709692118744', free_men_feudal_property_status, conventional).
narrative_ontology:cs_axiom('9724925c-1d69-4ae3-8261-709692118744', foundational, charter_feudal_contract_not_constitution).
narrative_ontology:cs_axiom_status(charter_feudal_contract_not_constitution, holdable).
narrative_ontology:cs_axiom_grounding('9724925c-1d69-4ae3-8261-709692118744', charter_feudal_contract_not_constitution, conventional).
narrative_ontology:cs_reference_frame('9724925c-1d69-4ae3-8261-709692118744', feudal_contract_1215_baronial_bargain).
narrative_ontology:cs_drift_state('9724925c-1d69-4ae3-8261-709692118744', magna_carta_1265_provisions_of_oxford, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('9724925c-1d69-4ae3-8261-709692118744', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, crown).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landholding_populace).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, rebel_barons).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The charter's named beneficiaries and primary contractors with the crown. They secure protection from arbitrary taxation, wardship abuse, and merchet exaction — extractive feudal incidents King John used to fund wars and suppress baronial power. Their 'free men' status grants them the charter's procedural protections (Clause 39: no arrest without lawful judgment). They coordinate with each other to enforce the contract against the crown's violations, but the protection set is bounded to their class interests: landholding, feudal privilege, and freedom from confiscatory royal incidents.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).

% Bound by the charter to refrain from the feudal incidents that funded royal authority: arbitrary taxation of baronial lands, wardship profits from minor heirs, merchet charges, and summary disinheritance. The crown's exit options are severely constrained — repudiation carries baronial rebellion and excommunication. The charter operates as a covenant with the barons, exchanging crown revenue-generation for baronial peace. John's immediate repudiation (July 1215) and the subsequent civil war demonstrate the contract's enforcement mechanism: credible threat of force.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, crown, payer,
    institutional, generational, trapped, national).

% Explicitly outside the charter's protection. The 'free men' clause (Clause 39: 'No free man shall be arrested...except by lawful judgment') applies only to men with feudal standing — landholders whose arrest would implicate baronial property claims and procedural rights. Commoners, villeins, and non-property-holding women have no standing under the charter and receive no explicit constraint on the crown's power. The charter's silence on the non-propertied majority is absolute: no protection, no coordination function for them.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landholding_populace, excluded,
    powerless, immediate, trapped, national).

% The Church's role is as charter enforcer and legitimacy granter. Pope Innocent III was central to the charter's creation — his representative negotiated the terms. The Church provides excommunication as a credible enforcement sanction against crown repudiation, placing the charter in the institutional frame of feudal obligation and divine authority. The Church's stake is in preventing excessive crown power that threatens clerical property and independence.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, ecclesiastical_authority, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, ecclesiastical_authority, observer).

% The northern barons (especially those whose lands were heavily taxed under feudal incidents) forced John to issue the charter in 1215. Their enforcement mechanism was military: they held London and threatened the crown's security. After John's repudiation and Pope Innocent III's excommunication of the rebels, their exit shifted — they remained militarily capable but lost ecclesiastical legitimacy, leading to the First Barons' War (1215–1217).
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, rebel_barons, payer,
    powerful, biographical, mobile, national).

% The charter itself, treated as a stabilized text and legal authority. Within this reading, the text is narrowly construed to its literal feudal meaning: 'free men' are the propertied class, the protections are bilateral (baron-crown), and absent provisions (silence on commoners) constitute exclusion. The charter is a fixed feudal contract, not a kernel to be reinterpreted.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, magna_carta_textual_authority, observer,
    analytical, civilizational, analytical, national).
narrative_ontology:stakeholder_non_agent(magna_carta_1215__baronial_privilege_reading, magna_carta_textual_authority).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the coordination problem of baronial rebellion by exchanging crown commitment to refrain from feudal incidents (arbitrary taxation, wardship abuse, merchet extraction) for baronial peace and tax revenue. Eliminates mutual uncertainty about the rules governing king-baron resource transfer.
% TRANSFER_FUNCTION: Transfers constraint on the crown's feudal revenue-raising capacity to the barons. The barons gain predictability about their feudal obligations; the crown loses income from arbitrary incidents but secures baronial compliance and war support. The non-propertied populace is not a party to the transfer — the charter is silent on their obligations and protections.
% ABSENT_VOICES: Every non-landholding person: commoners, villeins, unfree persons, and all women (both landholding and non-landholding). They are not in the room. A living-document or universal-rights reading would argue their silence is oppressive; a strict baronial reading treats it as irrelevant — the charter is a feudal contract between contracting parties, not a constitution for all England.
% DISAPPEARANCE_RATIONALE: If the baronial privilege reading of Magna Carta vanished overnight and its enforcement mechanisms dissolved, the crown's power to extract feudal incidents would return unchecked. Barons would face arbitrary wardship, merchet, and reliefs without negotiated limits. The result would be either renewed baronial rebellion or reimposition of John-era arbitrary rule. The charter coordinates the barons' collective power into a durable institutional arrangement; its disappearance restructures power back toward crown absolutism or renewed baronial war.
% FOUNDING_PROBLEM: King John's use of feudal incidents as revenue extraction without limit: wardship of minor heirs' lands, merchet charges on daughter marriages, relief demands on inheritance, and summary disinheritance of barons who displeased him. These incidents funded wars and punished baronial independence. The founding problem is the absence of a negotiated, contractual limit on arbitrary feudal revenue-raising by the crown.
% FOUNDING_PROBLEM_CORROBORATION: Chronicles of the period (Roger of Wendover, Matthew Paris) document baronial grievances over John's mercenary use of feudal incidents. The charter itself lists the incidents (wardship, merchet, relief) and sets explicit limits, confirming the founding problem. A universal-rights reading disputes that the founding problem is still live (it argues the real problem is arbitrary power over persons, not arbitrary incidents), but from within the baronial-privilege frame, the founding problem — unconstrained feudal revenue-raising — is what the charter directly addresses.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness metric (0.62, rising slightly then stabilizing) reflects that the charter operates as a constraint on one party (the crown) for the benefit of another (the barons). The extraction is moderate rather than high because the charter solves a genuine coordination problem — the barons avoid civil war costs, the crown avoids repeated baronial uprisings — making the constraint partially a coordination solution. However, the charter is extractive from the crown's perspective (lost feudal revenue) and profoundly extractive from the non-landholding populace's perspective (zero protection, zero standing). The suppression metric (0.71) reflects the active enforcement required to maintain the contract: baronial military threat at issuance, excommunication threat against repudiation, and the machinery of baronial power to enforce compliance over decades. Theater_ratio (0.28, rising then stabilizing) captures the growing gap between the charter's stated purpose (protecting 'free men') and its narrowing functional scope (protecting baronial property rights only). As later kings reissue the charter with modifications to expand the beneficiary set (1217 Magna Carta adds mention of merchants, 1225 version begins broader expansion), the original 1215 reading becomes increasingly theatrical — the words remain but their feudal-privilege meaning narrows relative to the reinterpretations. The measurements trace the charter's first 25 years: issuance (0), reissue under Henry III (5–10), and the Provisions of Oxford (15–25). Extraction stabilizes because the charter's core function — constraining arbitrary incidents — remains constant; theater rises because later reissues begin to gesture toward broader protections that contradict the narrow reading.
 *
 * PERSPECTIVAL GAP:
 *   The barons and the crown experience this constraint very differently. From the barons' seat: the charter is protection against arbitrary power, a coordinated solution to mutual security. From the crown's seat: the charter is a revenue constraint and a limit on prerogative, extractive of royal authority. From the non-landholding populace's seat: the charter is irrelevant — it offers no protection and explicitly excludes them from the 'free men' category. The engine will compute a different directional value (d) for each seat. The barons' d should be low (beneficiaries, mobile within their class, arbitrage access to alternative revenue sources through alliances). The crown's d should be high (target of the constraint, revenue-constrained, exit trapped by oath and military threat). The populace's d is ambiguous in this reading: they are not parties at all, so the constraint does not govern them directly, yet they are structurally disadvantaged by the barons' negotiated peace (which stabilizes feudal hierarchy and prevents the chaos that might create exit opportunities). This gap is the core structural insight: a 'liberalization' reading (universal rights) would argue for a different d for the populace; a baronial-privilege reading argues the constraint is simply inapplicable to them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary derivation: landowning_barons are the explicit named protectees of the charter; they collect security against arbitrary incidents, their power to enforce the contract is integral to its operation, and their exit options are constrained by mutual obligation (members of a feudal hierarchy cannot simply abandon the system). This yields low directionality (d ≈ 0.15–0.25, beneficiary end). Crown as victim: loses feudal revenue (merchet, wardship, relief can no longer be extracted arbitrarily), is bound by oath and excommunication threat, and cannot unilaterally repudiate without baronial war. High directionality (d ≈ 0.75–0.85, target end). The non-landholding populace are excluded from the constraint's scope, so directionality is technically undefined for them within this reading — they are not parties. However, if directionality is computed for excluded parties, it should reflect that the charter's existence constrains the scope of baronial demands on commoners (the barons' peace with the crown creates stability that reduces arbitrary violence on the populace) — paradoxically, the exclusionary reading might produce a low d (beneficiary) because the populace gains security from baronial peace, even though they were never named in the contract.
 *
 * MANDATROPHY ANALYSIS:
 *   The baronial-privilege reading avoids mandatrophy in 1215 — the founding problem (unconstrained feudal revenue extraction) is live, the barons are motivated to enforce the contract, and the charter addresses the stated problem directly. However, mandatrophy emerges over time (15–25 year mark) as later reissues (1217, 1225) expand the language and add clauses protecting merchants, clergy, and widows. By the mid-13th century (Provisions of Oxford, 1258), the charter becomes the framework for broader constitutional claims that explicitly include protections for categories not mentioned in 1215. From the baronial-privilege reading's perspective, this is mandate drift: the 1215 charter's function was to protect barons from arbitrary incidents, not to establish universal principles. The later expansions repurpose the text toward different ends, making the original constraint partly obsolete. A living-document reading celebrates this drift as constitutional development; the baronial reading views it as textual distortion. The measurement series captures this: theater_ratio rises (15–25) as the charter becomes increasingly invoked for purposes beyond its original feudal scope, but extraction stabilizes because the core revenue constraint on the crown persists regardless of reinterpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_men_category_boundary,
    'Does ''free man'' in Clause 39 denote exclusively men with feudal landholding status, or does it include any person not in villeinage or bondage?',
    'Historical linguistics analysis of ''liber homo'' in 13th-century documentary usage; comparison with non-charter sources of the period (chronicles, legal treatises, estate records) to determine whether the term consistently excludes commoners.',
    'If ''free man'' includes non-property-holders, the charter''s protections extend to a broader class, and the victim/beneficiary structure changes. If it is property-specific, the baronial reading stands. A universal reading depends on this boundary being permeable over time; the baronial reading depends on it being fixed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_men_category_boundary, empirical, 'Whether the charter''s protected class is defined by feudal property status or personal freedom status.').

omega_variable(
    charter_as_contract_vs_constitution,
    'Is Magna Carta a feudal contract between specific parties (king and barons) whose terms bind only those parties, or a proto-constitutional foundation whose principles apply broadly?',
    'Compare how the charter functioned in practice (1215–1265): was it enforced only against specific feudal incidents (wardship, merchet, relief) and only between the crown and baronial class, or did courts apply its language to non-feudal contexts and non-baronial plaintiffs?',
    'If contract: the baronial-privilege reading is structurally sound; the charter coordinates the king-baron relationship and excludes others. If constitution: the charter is a precedent for broader protections, and later expansions are not distortions but development. This is the fundamental interpretive fork.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(charter_as_contract_vs_constitution, conceptual, 'Whether the charter''s meaning is fixed to its original feudal context or open to constitutional reinterpretation.').

omega_variable(
    excluded_populace_silence,
    'Does the charter''s silence on commoners and unfree persons constitute deliberate exclusion (the populace is not protected because the king and barons deliberately chose not to protect them) or structural irrelevance (the charter does not address the populace because feudal contract language does not apply to non-contractual relations)?',
    'Examine baronial writings and negotiating records (if available) to determine whether the barons explicitly discussed and rejected protections for non-landholders, or whether the exclusion is incidental to the feudal-contract frame.',
    'If deliberate exclusion: the barons orchestrated the populace''s non-protection, and the charter is an extractive coordination that stabilizes inequality. If structural irrelevance: the charter simply does not address non-contractual relations, and a universal reading requires reframing the constraint entirely, not extending the existing one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_populace_silence, conceptual, 'Whether the populace''s exclusion from the charter reflects deliberate choice or structural limitation of the feudal-contract frame.').

omega_variable(
    papal_authority_source,
    'Does the Church''s role as enforcer (via excommunication threat) constitute legitimate authority-grounding for the charter, or is papal involvement an external coercive mechanism that does not address the charter''s internal legitimacy?',
    'Compare this charter''s ecclesiastical framing with other 13th-century feudal contracts to determine whether papal authority was standard in legitimizing bilateral feudal arrangements, or whether it is unusual and indicative of weakness in the charter''s baronial consensus.',
    'If ecclesiastical authority is standard: the charter''s legitimacy derives partly from the Church, and the reading''s authority_grounding is ''distributed'' (barons + crown + Church). If unusual: the reliance on papal backing indicates baronial doubt about the charter''s hold without external force, supporting a reading of the arrangement as extractive rather than freely coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(papal_authority_source, empirical, 'Whether the Church''s enforcement role indicates the charter''s fragility or standard feudal-ecclesiastical authority structure.').

omega_variable(
    kernel_reading_contest_boundary,
    'Are the three sibling readings (baronial_privilege, universal_rights, living_document) interpreting the SAME constraint or reframing the SAME text as three DIFFERENT constraints?',
    'Operational test: can a single constraint story model the charter''s behavior under all three readings, or must each reading instantiate a separate constraint with different ε, beneficiaries, and type? If the latter, the ''contest'' is actually a decomposition per the ε-invariance principle.',
    'If one constraint, three readings: the engine must compute a per-reading ε and per-seat directionality. If three constraints: the ε-invariance principle applies, and each reading is a separate constraint story linked via network.affects_constraints. The current author''s judgment (three separate constraints) rests on the claim that ε and victim/beneficiary structure differ materially: the baronial reading has narrow ε (feudal revenue constraint), narrow beneficiaries (barons), narrow victims (crown, populace as excluded); a universal reading would have higher ε (arbitrary power over all persons), universal beneficiaries (all persons), and universal victims (those lacking due process). These are not the same constraint measured differently — they have different referents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_boundary, conceptual, 'Whether competing readings of Magna Carta describe one constraint from multiple angles or decompose into multiple constraints per the ε-invariance principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1265).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__baronial_privilege_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(magn_tr_t0, observed).
narrative_ontology:measurement(magn_tr_t5, magna_carta_1215__baronial_privilege_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(magn_tr_t5, observed).
narrative_ontology:measurement(magn_tr_t10, magna_carta_1215__baronial_privilege_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(magn_tr_t10, observed).
narrative_ontology:measurement(magn_tr_t15, magna_carta_1215__baronial_privilege_reading, theater_ratio, 15, 0.3).
narrative_ontology:measurement_basis(magn_tr_t15, observed).
narrative_ontology:measurement(magn_tr_t20, magna_carta_1215__baronial_privilege_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(magn_tr_t20, observed).
narrative_ontology:measurement(magn_tr_t25, magna_carta_1215__baronial_privilege_reading, theater_ratio, 25, 0.28).
narrative_ontology:measurement_basis(magn_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement_basis(magn_be_t0, observed).
narrative_ontology:measurement(magn_be_t5, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement_basis(magn_be_t5, observed).
narrative_ontology:measurement(magn_be_t10, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement_basis(magn_be_t10, observed).
narrative_ontology:measurement(magn_be_t15, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(magn_be_t15, observed).
narrative_ontology:measurement(magn_be_t20, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 20, 0.62).
narrative_ontology:measurement_basis(magn_be_t20, observed).
narrative_ontology:measurement(magn_be_t25, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 25, 0.62).
narrative_ontology:measurement_basis(magn_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement_basis(magn_su_t0, observed).
narrative_ontology:measurement(magn_su_t5, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement_basis(magn_su_t5, observed).
narrative_ontology:measurement(magn_su_t10, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 10, 0.71).
narrative_ontology:measurement_basis(magn_su_t10, observed).
narrative_ontology:measurement(magn_su_t15, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(magn_su_t15, observed).
narrative_ontology:measurement(magn_su_t20, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(magn_su_t20, observed).
narrative_ontology:measurement(magn_su_t25, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(magn_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__baronial_privilege_reading, 0.12).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1225__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1225__living_document_reading).

% DUAL FORMULATION NOTE:
% The Magna Carta 1215 kernel is decomposed into three constraint stories per the ε-invariance principle: (1) baronial_privilege_reading — narrow feudal contract, feudal incident constraints, barons as sole beneficiaries, extraction moderate, suppression high; (2) universal_rights_reading — charter as transhistorical rights precedent, all persons as potential beneficiaries, arbitrary power as the core constraint, extraction high, suppression emergent; (3) living_document_reading — charter as constitutional substrate whose meaning evolves through interpretive tradition, beneficiaries expanding over time through reissue and reinterpretation, extraction shifting from feudal-revenue-focused to authority-erosion-focused. Each reading has a distinct ε referent (feudal revenue constraint vs. arbitrary power vs. interpretive authority), distinct victim/beneficiary structure, and distinct computed type. The three stories are linked via network.affects_constraints to show the constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__baronial_privilege_reading, powerless, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
