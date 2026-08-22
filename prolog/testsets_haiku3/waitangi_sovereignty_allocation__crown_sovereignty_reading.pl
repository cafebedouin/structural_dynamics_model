% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Treaty of Waitangi
 *   domain: constitutional/political/indigenous-rights
 *
 * SUMMARY:
 *   The Crown Sovereignty Reading interprets Article I of the Treaty of
 *   Waitangi (English text) as a complete and unqualified cession of
 *   sovereignty to the Crown. Under this reading, Māori chiefs ceded all
 *   legislative, executive, and judicial authority; the Crown thereafter
 *   exercises plenary power over all lands, resources, and peoples within
 *   Aotearoa without institutional requirement for Māori consent or
 *   partnership. This reading was dominant from 1840 through the 1970s,
 *   embedded in case law and parliamentary doctrine, and remains the formal
 *   constitutional framework despite significant institutional pressure from
 *   the partnership and rangatiratanga readings. The constraint is
 *   substantially extractive (0.89) because it allocates all sovereignty to a
 *   settler-colonial institution and renders Māori authority claims legally
 *   void. The theater ratio has risen over time (0.15→0.42) as the reading
 *   increasingly requires performative consultation, Treaty settlement
 *   bureaucracy, and symbolic recognition to maintain legitimacy while
 *   suppression requirements have declined (0.81→0.77) as the monopoly on
 *   effective enforcement has faced organized resistance.
 *
 * KEY AGENTS:
 *   - Crown Parliament: exercises unilateral legislative power; enforces this reading through courts and executive
 *   - British settler colonial administration: benefits from unrestricted resource access and land allocation under Crown sovereignty
 *   - Māori iwi and hapū: structurally powerless under this reading; trapped by legal doctrine of Crown supremacy
 *   - Waitangi Tribunal: operates within Crown sovereignty frame; can critique conduct but cannot overturn the reading
 *   - Partnership advocates: excluded from core enforcement; marginalized in dominant legal framework
 *   - Rangatiratanga advocates: most completely excluded; their authority claims dismissed as incompatible with sovereignty
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.89).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.77).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.89).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.77).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, snare).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Treaty of Waitangi").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional/political/indigenous-rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'd5de2732-5c05-48cf-9a33-abe3823288aa').
narrative_ontology:cs_kernel_codification('d5de2732-5c05-48cf-9a33-abe3823288aa', fixed_text).
narrative_ontology:cs_authority_grounding('d5de2732-5c05-48cf-9a33-abe3823288aa', extraction).
narrative_ontology:cs_interpretation_layer_present('d5de2732-5c05-48cf-9a33-abe3823288aa').
narrative_ontology:cs_reading_relation('d5de2732-5c05-48cf-9a33-abe3823288aa', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_reading_relation('d5de2732-5c05-48cf-9a33-abe3823288aa', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_axiom('d5de2732-5c05-48cf-9a33-abe3823288aa', foundational, english_article_i_absolute_cession).
narrative_ontology:cs_axiom_status(english_article_i_absolute_cession, holdable).
narrative_ontology:cs_axiom_grounding('d5de2732-5c05-48cf-9a33-abe3823288aa', english_article_i_absolute_cession, conventional).
narrative_ontology:cs_axiom('d5de2732-5c05-48cf-9a33-abe3823288aa', foundational, crown_parliamentary_supremacy).
narrative_ontology:cs_axiom_status(crown_parliamentary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d5de2732-5c05-48cf-9a33-abe3823288aa', crown_parliamentary_supremacy, conventional).
narrative_ontology:cs_axiom('d5de2732-5c05-48cf-9a33-abe3823288aa', secondary, maori_consent_legitimates_subordination).
narrative_ontology:cs_axiom_status(maori_consent_legitimates_subordination, overridden).
narrative_ontology:cs_axiom_grounding('d5de2732-5c05-48cf-9a33-abe3823288aa', maori_consent_legitimates_subordination, deontological).
narrative_ontology:cs_reference_frame('d5de2732-5c05-48cf-9a33-abe3823288aa', westminster_crown_sovereignty_1840).
narrative_ontology:cs_drift_state('d5de2732-5c05-48cf-9a33-abe3823288aa', contemporary_2025, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d5de2732-5c05-48cf-9a33-abe3823288aa', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_settler_colonial_administration).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_treaty_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_treaty_claimants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Westminster Parliament, as the seat of Crown sovereignty under this reading, exercises plenary legislative power over all lands, resources, and Māori affairs within the territory. Parliament sets the legal framework interpreting the Treaty, allocates Crown resources, and enforces sovereignty through courts and executive administration. Benefits from undisputed control and the ability to alter terms unilaterally.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament, agenda_setter,
    institutional, generational, arbitrage, national).

% Colonial administrators and settler interests benefit from unrestricted legal access to Māori lands and resources. Under this reading, they face no institutional requirement to negotiate or consult; Crown unilateral authority enables expropriation and development. Gains flow directly to settler institutions through land confiscation, resource rents, and policy-making power.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, british_settler_colonial_administration, beneficiary,
    institutional, biographical, mobile, national).

% Indigenous iwi and hapū are subordinated to parliamentary will under this reading. Their authority claims over lands, resources, and taonga are deemed legally void. They cannot veto legislation affecting their territories; their remedies are limited to Parliamentary grace or litigation within courts that apply the Crown sovereignty interpretation. Trapped by legal doctrine, geographic immobility, and the closure of exit options within the Westminster system.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_and_hapu, payer,
    powerless, generational, trapped, national).

% Seek redress through the Waitangi Tribunal and courts for historical wrongs and ongoing injustices. Operate within a legal framework that begins by accepting Crown sovereignty; their claims are framed as requests for discretionary remedy rather than assertions of retained authority. The Tribunal provides a forum for grievance articulation (partial beneficiary role) but constrained within the supremacy doctrine that operates as their payer role.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_treaty_claimants, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_treaty_claimants, beneficiary).

% An advisory body established by Parliament (under this reading) to investigate Treaty claims and make recommendations. Operates within the Crown sovereignty frame as its default legal condition: can critique Crown conduct but cannot declare aspects of the sovereignty interpretation void without Parliament's consent. Acts as an analytical observer and partial pressure valve for grievance.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal, observer,
    institutional, biographical, constrained, national).

% Judges, legal scholars, and political movements advocating the partnership reading of the Treaty are excluded from the core enforcement mechanism of the Crown sovereignty reading. Their interpretations are marginalized within dominant parliamentary supremacy doctrine, though they have gained institutional footholds in certain judicial decisions and statutory consultation requirements that chip away at the reading's edges without formally overthrowing it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, partnership_advocates, excluded,
    organized, generational, constrained, national).

% Māori-led movements asserting tino rangatiratanga (full retained authority over lands and resources) are most completely excluded from this reading's enforcement frame. Their authority claims are dismissed as incompatible with Crown sovereignty; within courts and Parliament applying this reading, rangatiratanga assertions face complete structural denial. Identity-locked because assertion of tino rangatiratanga is central to Māori political and cultural identity.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatiratanga_advocates, excluded,
    powerless, generational, identity_locked, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, crown_parliament).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, centralized legal authority (Westminster Parliament) to make law for the entire territory and all inhabitants, replacing pre-existing plural indigenous authority systems with a single sovereign hierarchical system. Solves the logistical problem of legal pluralism by subordinating all alternative authorities to one supreme source.
% TRANSFER_FUNCTION: Moves sovereignty, legislative authority, and resource-allocation power from Māori iwi and hapū to the Crown, thence to British and settler colonial institutions. Transfers the legal capacity to govern, tax, confiscate lands, extract resources, and set policy on Māori affairs entirely to Parliament. The flow runs from Māori (target) to Crown/settlers (beneficiary).
% ABSENT_VOICES: The rangatiratanga reading (Māori text Article II interpretation asserting Māori retained tino rangatiratanga) and the partnership reading (Crown-Māori partnership requiring good-faith consultation) are structurally excluded from this reading's enforcing institutions. Advocates for those readings are denied standing to argue that this reading misinterprets the Treaty or misallocates sovereignty. Māori authority claimants are excluded because their premises directly contradict the Crown sovereignty axiom; the exclusion is built into the reading's logical structure, not incidental to it.
% DISAPPEARANCE_RATIONALE: If this reading disappeared — if courts and Parliament adopted a partnership or rangatiratanga reading instead — the entire legal structure of Crown resource allocation, land ownership, and policy-making would destabilize. Unilateral Crown authority over Māori affairs would collapse into a genuine partnership framework or Māori-retained authority; resource control would face immediate redistribution claims; decision-making would require power-sharing or Māori veto. The territorial governance structure would reorganize. The constraint is actively maintained precisely because its disappearance would rearrange power dramatically.
% FOUNDING_PROBLEM: Establish singular Crown legislative authority over a territory containing multiple indigenous iwi with pre-existing authority systems; create a centralized legal framework permitting unified settler colonial administration without ongoing obligation to negotiate or share power with indigenous authorities.
% FOUNDING_PROBLEM_CORROBORATION: Crown Parliament and settler institutional beneficiaries affirm the founding problem remains live. Independent legal historians, rangatiratanga advocates, and partnership advocates unanimously argue the founding problem is dead: it was manufactured by treating indigenous governance as an obstacle rather than a reality to negotiate with; it is now recognized as fundamentally unjust, and even within the settler state there is majority political consensus that Crown unilateral authority over Māori affairs is illegitimate. The Waitangi Tribunal's own jurisprudence (post-1975) has moved away from affirming this problem's currency. International human rights law now frames the problem as the existence of Crown sovereignty rather than its absence. Corroboration from all seats outside the Crown beneficiary seat indicates the founding problem is obsolete.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.89, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.89) because the reading allocates sovereignty entirely to an external institution (Crown Parliament) and renders the victim group (Māori iwi) legally incompetent to govern themselves. Suppression is substantial (0.77) because maintaining this allocation requires active enforcement: courts must repeatedly dismiss Māori authority claims, Parliament must resist alternative readings, and the legal doctrine of Crown sovereignty must be defended through education, case law, and constitutional messaging. The theater ratio's rise over time (from 0.15 to 0.42) reflects a striking pattern: as political resistance mounted (especially from the 1970s onward), the Crown adopted performative recognition — the Waitangi Tribunal (1975), Treaty settlement processes, consultation requirements, statutory acknowledgments — without fundamentally altering the underlying sovereignty allocation. These performances absorbed some resistance energy while preserving the core extraction. The temporal series captures this dynamic: extractiveness dipped in 1990 (the settlement boom era) as theatrical recognition peaked (0.48), then rebounded as the core supremacy was reasserted (2025 projection: 0.89). This is a canonical pattern of Goodhart drift on a snare: the performance metrics rise while the extraction metric regenerates.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown Parliament and settler colonial seats, this reading appears as a clean, legitimate constitutional arrangement: a unified sovereign authority exercising proper stewardship over the entire territory. From the Māori iwi seat, the identical reading appears as an absolute extraction mechanism: their authority, lands, and decision-making power were taken without genuine consent and remain unavailable despite a century of resistance. The rangatiratanga advocates experience this reading as an active denial of Māori sovereignty that was never ceded (or ceded only kāwanatanga, not tino rangatiratanga). The engine's per-seat computation should surface these radical perspectival divides: the same reading, dramatically different directionality values across seats. The Crown's d approaches 0 (full beneficiary); Māori iwi and hapū approach 1.0 (full target); partnership advocates sit in the middle, experiencing both coordination benefit (a unified legal system) and extraction harm (subordination within that system).
 *
 * DIRECTIONALITY LOGIC:
 *   Crown Parliament is the clear beneficiary: it receives full sovereignty, unilateral resource allocation authority, and the power to set the legal terms of its own legitimacy. Settler colonial institutions benefit from unrestricted access to Māori lands under a legal regime that subordinates indigenous claims. Māori iwi and hapū are full targets: they lose authority, land, and decision-making power, with no institutional mechanism to recover it or veto its use. The power atom differentials are crucial: the Crown and settler institutions are institutional/powerful; Māori iwi are powerless in the Westminster system and identity-locked (their authority claims are embedded in their identity as iwi leaders, making exit unthinkable). This creates an extreme directionality gap: d for Crown ≈ 0.05 (arbitrage exit, institutional power, beneficiary role); d for Māori iwi ≈ 0.95 (trapped exit, powerless position, payer role, identity-locked). The Waitangi Tribunal occupies a symmetric or slightly beneficiary position: it coordinates some accountability (the partnership reading's influence) while operating within the Crown sovereignty frame that constrains it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading displays active mandatrophy across the 1960–1990 period. The founding problem — establishing unified Crown legislative authority — remains nominally live (the Crown affirms it), but the problem's legitimacy collapsed by the 1970s as Māori activism, legal scholarship challenging the Crown sovereignty interpretation, and international indigenous rights movements delegitimized the foundational premise (that Māori consent was real or that indigenous authority should be erased). The theater ratio's spike (0.35→0.48 across 1960–1990) and the drop in suppression requirement (0.84→0.68) reflect exactly this trajectory: the reading had to adopt performative mechanisms (Tribunal, settlement process, consultation rhetoric) to survive delegitimation. The suppression rebounded in later years (0.68→0.77 projected at 2025) as the Crown reasserted core sovereignty claims in response to rangatiratanga and partnership movements gaining institutional power. This is mandatrophy resolution in real time: the original mandate is dead (nobody seriously argues Māori fully consented to permanent subordination), but the constraint persists through bureaucratic performance and renewed enforcement against rival readings. The settlement system itself is a canonical mandatrophy artifact: it performs remedy and partnership while leaving Crown sovereignty untouched.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    english_text_vs_maori_text_divergence,
    'Did Māori signatories in 1840 understand Article I (English: ''ceded absolute sovereignty'') the same way as English signatories, or did the Māori text Article II (retaining ''tino rangatiratanga'') represent a fundamentally different commitment that Māori chiefs believed they were making?',
    'Historical linguistics, oral histories from iwi, comparative analysis of the English and Māori texts as legal instruments, and Māori testimony about what was understood to be conveyed in 1840.',
    'If Māori signatories understood tino rangatiratanga retention, this reading''s ε should collapse (from 0.89 toward 0.55) as the legal foundation (genuine Māori consent to absolute cession) dissolves. The reading would reclassify from snare to contested_extraction or false_summit. If Māori signatories also understood absolute cession, this reading''s legitimacy strengthens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(english_text_vs_maori_text_divergence, empirical, 'The foundational factual ambiguity: did the signatories understand the same commitment?').

omega_variable(
    coercion_vs_voluntary_agreement,
    'To what extent was the 1840 agreement itself coerced — Māori chiefs acting under duress, military threat, or unequal information — rather than voluntary?',
    'Archival evidence of Crown military posture, testimony about what Māori chiefs knew of Crown intentions, comparative analysis with other land-cession treaties of the period to establish coercion baseline.',
    'Genuine coercion would undermine the entire premise of legitimate sovereignty cession. If the agreement was coerced, the Crown''s legitimacy to enforce this reading collapses and alternative readings gain mandatrophy traction. This omega addresses whether consent was real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_voluntary_agreement, empirical, 'Whether the 1840 agreement was coerced or genuinely voluntary.').

omega_variable(
    sovereignty_retention_through_non_exercise,
    'If Māori iwi have continuously exercised authority (over hapū affairs, taonga management, community decisions) for 185 years despite Crown claims to plenary sovereignty, does continuous non-exercise of plenary power by the Crown constitute effective abandonment or waiver of the sovereignty claim?',
    'Legal doctrine on sovereignty abandonment; empirical mapping of domains where Crown actually exercised plenary power vs. domains where Māori authority continued de facto; examination of whether ''Crown sovereignty'' is now more theoretical than operational in key dimensions.',
    'If abandonment doctrine applies, this reading''s ε should drop substantially: the Crown claims sovereignty but has not actually exercised it, suggesting the claim is performance rather than substantive extraction. The theater ratio would become the dominant metric. If abandonment does not apply, the reading''s structure remains intact but the theater component becomes even more salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_retention_through_non_exercise, conceptual, 'Whether 185 years of Māori non-interference constitutes implicit sovereignty waiver.').

omega_variable(
    mandatory_vs_permissive_partnership,
    'Can this reading coexist with the partnership reading, or does the Crown sovereignty reading''s logical structure foreclose mandatory partnership obligations?',
    'Analysis of whether Parliament can unilaterally adopt consultation requirements and partnership performance without formally yielding sovereignty, or whether consultation creates de facto power-sharing that undermines the pure sovereignty claim.',
    'If partnership obligations undermine rather than merely constrain sovereignty, the boundary between this reading and partnership collapses; the constraint becomes hybrid. If Parliament can maintain unilateral sovereignty while adopting partnership performances, the two readings truly coexist and the theater ratio becomes the key differentiator.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatory_vs_permissive_partnership, conceptual, 'Whether mandatory partnership is compatible with absolute Crown sovereignty, or whether it functionally forecloses it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.15).
narrative_ontology:measurement_basis(wait_tr_t1840, observed).
narrative_ontology:measurement(wait_tr_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1880, 0.18).
narrative_ontology:measurement_basis(wait_tr_t1880, observed).
narrative_ontology:measurement(wait_tr_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1920, 0.22).
narrative_ontology:measurement_basis(wait_tr_t1920, observed).
narrative_ontology:measurement(wait_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.35).
narrative_ontology:measurement_basis(wait_tr_t1960, observed).
narrative_ontology:measurement(wait_tr_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1990, 0.48).
narrative_ontology:measurement_basis(wait_tr_t1990, observed).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2025, 0.42).
narrative_ontology:measurement_basis(wait_tr_t2025, projected).

% Extraction over time
narrative_ontology:measurement(wait_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.85).
narrative_ontology:measurement_basis(wait_be_t1840, observed).
narrative_ontology:measurement(wait_be_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1880, 0.91).
narrative_ontology:measurement_basis(wait_be_t1880, observed).
narrative_ontology:measurement(wait_be_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1920, 0.92).
narrative_ontology:measurement_basis(wait_be_t1920, observed).
narrative_ontology:measurement(wait_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement_basis(wait_be_t1960, observed).
narrative_ontology:measurement(wait_be_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement_basis(wait_be_t1990, observed).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2025, 0.89).
narrative_ontology:measurement_basis(wait_be_t2025, projected).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.65).
narrative_ontology:measurement_basis(wait_su_t1840, observed).
narrative_ontology:measurement(wait_su_t1880, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1880, 0.81).
narrative_ontology:measurement_basis(wait_su_t1880, observed).
narrative_ontology:measurement(wait_su_t1920, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1920, 0.84).
narrative_ontology:measurement_basis(wait_su_t1920, observed).
narrative_ontology:measurement(wait_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.76).
narrative_ontology:measurement_basis(wait_su_t1960, observed).
narrative_ontology:measurement(wait_su_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement_basis(wait_su_t1990, observed).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2025, 0.77).
narrative_ontology:measurement_basis(wait_su_t2025, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.18).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the Treaty of Waitangi kernel family (three readings: crown_sovereignty, partnership, rangatiratanga). The three stories share a single kernel (the Treaty text) but author structurally distinct constraints with different ε values, beneficiary/victim structures, and temporal trajectories. The crown_sovereignty_reading maintains the highest ε (0.89) and embeds extractiveness as core function; the partnership_reading adds consultation obligations that moderate extraction; the rangatiratanga_reading inverts the whole structure (Māori retain authority). Network edges encode causal/logical dependency: crown_sovereignty forecloses rangatiratanga and influences partnership by setting the baseline from which partnership advocates negotiate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, powerless, 0.95).
constraint_indexing:directionality_override(waitangi_sovereignty_allocation__crown_sovereignty_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
