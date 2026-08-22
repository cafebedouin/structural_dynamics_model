% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__rangatiratanga_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__rangatiratanga_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__rangatiratanga_reading
 *   human_readable: Māori Tino Rangatiratanga (Article II, Māori Text) — Rangatiratanga Reading
 *   domain: constitutional/indigenous_rights/post-colonial_governance
 *
 * SUMMARY:
 *   The Māori text of Article II of the Treaty of Waitangi (1840) grants
 *   'tino rangatiratanga' (full chiefly authority) to Māori over their lands,
 *   resources, and sacred taonga (treasures), with the Crown receiving only
 *   'kāwanatanga' (governorship) limited to settler administration. This
 *   reading asserts that Māori retained inherent sovereignty over their
 *   territories and peoples, with Crown jurisdiction limited to non-Māori
 *   populations. In practice, Crown institutions have operationalized the
 *   English Article I reading (Crown absolute sovereignty) instead, treating
 *   Article II as granting only internal administrative authority subordinate
 *   to Crown supremacy. The rangatiratanga reading represents the
 *   authoritative position of Māori iwi, supported by linguistic analysis of
 *   the Māori text and indigenous legal scholarship, but is suppressed by
 *   Crown institutional authority. The extraction here is the continuous
 *   negation of a nominally granted authority through legislative and
 *   judicial assertion of overriding Crown sovereignty. The theater ratio
 *   rises sharply after t=60 as Crown governance rhetoric increasingly
 *   invokes partnership, consultation, and co-management while maintaining
 *   institutional control — performative recognition of rangatiratanga
 *   without surrendering the authority claimed.
 *
 * KEY AGENTS:
 *   - maori_iwi_collectives: Organized political actors asserting rangatiratanga claim; have moderate power within iwi structure, high identity lock, trapped territorial scope
 *   - crown_institutional_authority: Institutional actor asserting sovereignty; controls enforcement machinery (courts, police, resource management agencies); has institutional power and arbitrage exit
 *   - traditional_territory_holders: Distributed moderate-power agents bearing cost of dual legal regimes; identity-locked to specific rohe (territories)
 *   - settler_population: Powerful beneficiary of Crown sovereignty reading; mobile exit; benefits from unified Crown law without recognizing Māori authority
 *   - crown_courts_and_enforcement: Institutional mechanism for operationalizing Crown reading; interprets Article II as subordinate to Article I
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68).
domain_priors:suppression_score(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.71).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__rangatiratanga_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__rangatiratanga_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__rangatiratanga_reading, "Māori Tino Rangatiratanga (Article II, Māori Text) — Rangatiratanga Reading").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__rangatiratanga_reading, "constitutional/indigenous_rights/post-colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__rangatiratanga_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__rangatiratanga_reading, '5a6370b8-8769-4bc0-afa9-3c106dc600c4').
narrative_ontology:cs_kernel_codification('5a6370b8-8769-4bc0-afa9-3c106dc600c4', fixed_text).
narrative_ontology:cs_authority_grounding('5a6370b8-8769-4bc0-afa9-3c106dc600c4', extraction).
narrative_ontology:cs_interpretation_layer_present('5a6370b8-8769-4bc0-afa9-3c106dc600c4').
narrative_ontology:cs_reading_relation('5a6370b8-8769-4bc0-afa9-3c106dc600c4', waitangi_sovereignty_allocation__crown_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('5a6370b8-8769-4bc0-afa9-3c106dc600c4', waitangi_sovereignty_allocation__partnership_reading, influences).
narrative_ontology:cs_axiom('5a6370b8-8769-4bc0-afa9-3c106dc600c4', foundational, maori_inherent_sovereignty_retained).
narrative_ontology:cs_axiom_status(maori_inherent_sovereignty_retained, holdable).
narrative_ontology:cs_axiom_grounding('5a6370b8-8769-4bc0-afa9-3c106dc600c4', maori_inherent_sovereignty_retained, deontological).
narrative_ontology:cs_axiom('5a6370b8-8769-4bc0-afa9-3c106dc600c4', foundational, kawanatanga_limits_crown_to_settler_governance).
narrative_ontology:cs_axiom_status(kawanatanga_limits_crown_to_settler_governance, holdable).
narrative_ontology:cs_axiom_grounding('5a6370b8-8769-4bc0-afa9-3c106dc600c4', kawanatanga_limits_crown_to_settler_governance, empirically_contingent).
narrative_ontology:cs_axiom('5a6370b8-8769-4bc0-afa9-3c106dc600c4', secondary, maori_text_primacy_in_treaty_interpretation).
narrative_ontology:cs_axiom_status(maori_text_primacy_in_treaty_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('5a6370b8-8769-4bc0-afa9-3c106dc600c4', maori_text_primacy_in_treaty_interpretation, deontological).
narrative_ontology:cs_reference_frame('5a6370b8-8769-4bc0-afa9-3c106dc600c4', maori_inherent_rangatiratanga_reserved).
narrative_ontology:cs_drift_state('5a6370b8-8769-4bc0-afa9-3c106dc600c4', contemporary_crown_institutional_control, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('5a6370b8-8769-4bc0-afa9-3c106dc600c4', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collectives).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collective_authority).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__rangatiratanga_reading, traditional_territory_holders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Iwi and hapū seek to exercise the tino rangatiratanga (full authority) Article II nominally granted them over lands, resources, and sacred taonga. They interpret the Māori text as reserving Crown jurisdiction only to settler populations (kāwanatanga), leaving Māori self-governance over their own people and territories intact. Their authority is constitutionally recognized but operationally constrained by 150+ years of Crown assertion of overriding sovereignty. Exit is identity-locked: the claim of rangatiratanga is inseparable from Māori political identity and cannot be abandoned without dissolving the collective self.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collectives, agenda_setter,
    organized, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_iwi_collectives, beneficiary).

% The Crown asserts that Article I (English text) conveyed absolute sovereignty, which English Article II operationalized through the Governor's explicit right to 'exclusive' purchasing of Māori land. The Crown framework treats Article II Māori text's 'kāwanatanga' as a reassurance to Māori that they retained authority over internal affairs, but subordinate to Crown sovereignty. Crown enforcement machinery (courts, police, resource management law) continuously enforces this reading. The Crown maintains it also benefits Māori through rule of law, public infrastructure, and legal protection — a coordination framing the rangatiratanga reading contests.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutional_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Iwi and hapū with rohe (territories) and resource rights under customary law cannot exercise those rights without Crown consent under the Resource Management Act, conservation law, and property law frameworks that override customary authority. They bear the cost of dual, conflicting legal regimes: Crown law that excludes Māori authority from decision-making on Crown land and marine space, and customary law that claims authority but lacks enforcement machinery. They cannot exit: their identity and whakapapa (genealogy) are constituted through relationship to specific rohe.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, traditional_territory_holders, payer,
    moderate, generational, trapped, regional).

% The rangatiratanga reading asserts Māori collective authority over law-making, resource allocation, and governance within their territories. This authority is continuously undercut by Crown legislation that asserts paramountcy, by resource allocation mechanisms that give Crown override power, and by funding structures that make Iwi authorities fiscally dependent on Crown appropriations. Māori governance bodies (Iwi councils, hapū kaitiaki groups) operate under Crown-delegated powers rather than inherent authority, which constrains their decisions and makes them susceptible to Crown defunding. Their authority is theoretically retained but practically extracted through financial and legal dependency.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, maori_collective_authority, payer,
    organized, generational, identity_locked, national).

% Under the Crown sovereignty reading (the operative framework), settlers benefit from unified Crown law, property ownership protections, democratic voting rights, and access to Crown-controlled resources without needing to negotiate with Māori authorities. The rangatiratanga reading would require settlers to recognize Māori authority in traditional territories and negotiate resource use and governance with Iwi. Settlers' primary interest is in the status quo (Crown law, defined property rights, unified governance). They have the option of political exit (disengagement from treaty settlement processes) but typically exercise it.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, settler_population, beneficiary,
    powerful, biographical, mobile, national).

% Courts interpret the Treaty through English Article I language and Common Law doctrine, treating Article II kāwanatanga as internal Māori administration subordinate to Crown sovereignty. The Resource Management Act, Conservation Act, and Property Law Act encode Crown legislative supremacy, making courts the enforcement mechanism for Crown sovereignty over Māori authority claims. Judicial interpretation has gradually shifted toward recognizing Māori rights, but courts remain the venue through which Crown authority is articulated and Māori authority claims are evaluated for compliance with Crown law.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_courts_and_enforcement, agenda_setter,
    institutional, generational, analytical, national).

% Legal scholars, Māori historians, and indigenous rights advocates argue that the Māori text of Article II unambiguously reserves tino rangatiratanga (full chiefly authority) to Māori. They point to 19th-century correspondence and Māori understanding at the time of signing. They are excluded from final authority over constitutional interpretation: Crown courts hold the institutional power to define Treaty meaning, and their voice enters only through litigation and advisory opinions that courts may or may not accept. Their exclusion is maintained by assigning 'historical context' lower epistemic weight than 'plain language of negotiated English text' in judicial methodology.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, rangatiratanga_scholars_and_advocates, excluded,
    moderate, biographical, constrained, national).

% UN Permanent Forum on Indigenous Issues, ILO Convention 169 bodies, and international human rights mechanisms observe and critique New Zealand's implementation of the Treaty. They affirm the rangatiratanga reading as consistent with indigenous rights law but have no enforcement power over Crown actions. Their observations serve as external corroboration for the rangatiratanga reading and as pressure on Crown legitimacy, but do not constrain Crown enforcement machinery.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__rangatiratanga_reading, international_indigenous_rights_bodies, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__rangatiratanga_reading, crown_institutional_authority).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__rangatiratanga_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the coexistence of two legal orders — Māori customary law and governance in traditional territories, Crown law for settler administration and resource management — through a framework that nominally acknowledges both but operationally prioritizes Crown sovereignty. The rangatiratanga reading treats this as a functional arrangement only if authority is genuinely shared; the Crown reading treats it as coordination that places Māori under Crown protection while reserving Crown final authority.
% TRANSFER_FUNCTION: Transfers de facto governance authority from Māori collectives to Crown institutions; transfers resource allocation rights from traditional rights-holders to Crown-administered agencies; transfers revenues and rents from Crown-controlled resource exploitation to the Crown (and settler landholders) rather than to Iwi; transfers legitimacy and narrative control to Crown legal doctrine by making Crown interpretation of Article II the operative meaning despite textual ambiguity. In the rangatiratanga reading, this transfer is extraction — the authority nominally reserved was not actually granted, sovereignty was retained under false pretense.
% ABSENT_VOICES: The Māori negotiators and chiefs who signed Article II and understood 'tino rangatiratanga' as full authority are absent (deceased by 1900). Māori-language speakers and Māori scholars who argue the Māori text's plain meaning are excluded from institutional authority to define what Article II means — that authority is held by Crown courts which privilege English text and Westminster doctrine. Māori communities in territories affected by Crown resource management decisions are excluded from veto power despite Article II's stated reservation. Settler populations opposed to recognizing Māori authority are politically mobile and their voice is heard; excluded Māori voices are those who would insist on operationalizing rangatiratanga.
% DISAPPEARANCE_RATIONALE: If Crown assertion of sovereignty over the rangatiratanga reading were abandoned and the reading operationalized, resource management, land law, and governance structures would reorganize substantially: Māori Iwi authorities would gain veto power over resource extraction in traditional territories; dual governance structures would emerge; Crown law would need Crown-Māori negotiation for implementation in Māori-majority areas; property law around Crown land and marine space would shift toward Iwi co-management or cession. The entire post-1840 institutional architecture of New Zealand governance rests on the Crown sovereignty reading; acceptance of rangatiratanga as the binding reading would force institutional redesign.
% FOUNDING_PROBLEM: 1840 encounter between British Empire seeking to establish sovereignty and Māori iwi seeking to maintain their chiefly authority while gaining access to British trade and protection. Article II was negotiated as a statement that Māori would retain their own governance and property while accepting Crown jurisdiction over settlers and inter-group disputes. The problem was: how to establish Crown legitimacy without destroying the Māori political order that Crown needed to be allied with?
% FOUNDING_PROBLEM_CORROBORATION: Māori iwi leaders and contemporary Māori accounts attest that Article II was understood as a preservation of mana (authority) and rangatiratanga; Māori negotiators explicitly sought language that would prevent Crown from controlling Māori lands and people. Crown officials at signing recorded mixed messaging — some acknowledged Māori retained authority, others claimed Crown gained full sovereignty. Modern scholarship outside the Crown legal establishment (indigenous legal scholars, historians, linguists) corroborates the Māori-text reading: the Māori text's 'tino rangatiratanga' is not the same thing as 'internal affairs only' — it is full authority. The founding problem (maintaining Crown legitimacy without destroying Māori order) has been 'solved' by the Crown reading, but that solution required suppressing the rangatiratanga reading, which suggests the solution was not consensual and rests on institutional power rather than genuine coordination.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__rangatiratanga_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__rangatiratanga_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__rangatiratanga_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__rangatiratanga_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__rangatiratanga_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at interval end) is high because the constraint systematically transfers authority, resources, and governance rights from Māori collectives to Crown institutions, despite nominally granting rangatiratanga in Article II. Suppression (0.71) is higher because Crown maintains this extraction through legislative paramountcy, judicial interpretation prioritizing English text, and funding dependency of Māori governance bodies. The measurement series shows extraction rising steeply from t=0 (pre-1840 baseline, rangatiratanga imagined as operative) through t=90 (circa 1930s, full Crown sovereignty consolidated), then plateauing at t=120+ (post-WWII era of rhetorical partnership while institutional control persists). Theater ratio rises from near-zero (early Crown control was overt appropriation, no performance) to 0.62 (contemporary era: Crown-Māori co-management structures, consultation requirements, treaty settlement process) — the shift reflects Crown increasingly performing recognition of rangatiratanga while retaining veto power and enforcement authority. Accessibility collapse is low (0.48) because the rangatiratanga reading remains a coherent alternative claimed by Māori iwi, international indigenous rights bodies, and constitutional scholars — it has not been eliminated as an interpretive option, only suppressed institutionally. Resistance is high (0.74) because Māori iwi, settlement movements, and advocacy groups continuously contest the Crown sovereignty reading and assert rangatiratanga claims in litigation, political organizing, and claims negotiation.
 *
 * PERSPECTIVAL GAP:
 *   From the Māori iwi seat: rangatiratanga is a retained right being continuously extracted through Crown assertion of overriding sovereignty. From the Crown institutional seat: kāwanatanga is properly understood as internal Māori administration subordinate to Crown sovereignty for the greater good (unified law, rule of law, public infrastructure). From the settler seat: the Crown sovereignty reading provides clarity, property security, and access to Crown-controlled resources without needing to negotiate with Māori authorities; the rangatiratanga reading would impose transaction costs and governance complexity. These perspectives compute different effective extractiveness (d) values: for iwi, d approaches 1.0 (full target of extraction); for Crown institutions, d approaches 0.0 (beneficiary, defending its authority and rents); for settlers, d is low (beneficiaries of Crown law). The engine computes these per-seat, reflecting the structural fact that the same constraint operates as coordination (Crown framing) or extraction (Māori framing) depending on which party's authority is recognized.
 *
 * DIRECTIONALITY LOGIC:
 *   Māori iwi collectives are the nominal beneficiaries of Article II (granted tino rangatiratanga) but are de facto payers (their authority is continuously negated by Crown assertion). Directionality for this agent should be high (d near 1.0) — they are the target of the extraction. Crown institutions are the agenda-setters and beneficiaries (collect authority, rents, and legitimacy from the Crown sovereignty reading); directionality should be low (d near 0.0). Settler population benefits from unified Crown law without negotiating with Māori; directionality is beneficiary-side. Traditional territory holders are trapped payers (cannot exit; identity-locked to rohe; face dual legal regimes that exclude their customary authority); directionality is high-target (d near 1.0). The measurement of directionality should track the shift from nominal equality (Article II grants authority to Māori) to practiced inequality (Crown enforcement negates that authority), which the rising extraction curve reflects.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Crown legitimacy without destroying Māori political order) was 'solved' by Crown assertion of absolute sovereignty, which violated the nominal solution (rangatiratanga retained). This is a mandatrophy scenario: the arrangement persists, but it persists through suppression rather than through genuine coordination on the problem. The problem still exists (Crown needs legitimacy; Māori seek authority), but the constraint's original mandate (establish Crown legitimacy consensually while preserving Māori rangatiratanga) has been abandoned in practice. Contemporary treaty settlement processes and co-management frameworks are theatrical responses to the mandatrophy: they perform recognition of rangatiratanga while maintaining Crown institutional control. The constraint would be reclassifiable as piton (atrophied function, theatrical performance of the coordinate solution) if the performance were the primary feature; instead it remains tangled_rope because the extraction (authority denial) is the actual function and the performance (consultation, co-management rhetoric) is secondary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    maori_text_interpretation_finality,
    'Is the Māori text of Article II the authoritative version of the Treaty for interpreting Māori sovereign rights, or is it subsidiary to the English text and Crown legislative intent?',
    'Constitutional amendment or landmark court decision establishing Māori text as primary authority. Alternatively, historical linguistic analysis establishing what Māori signatories understood ''tino rangatiratanga'' to mean in 1840 context.',
    'If Māori text is primary, the rangatiratanga reading becomes the authoritative constraint and Crown sovereignty becomes the contested reading. If English text remains primary, the rangatiratanga reading remains suppressed. This is the foundational question underlying all Treaty interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maori_text_interpretation_finality, conceptual, 'Whether Māori text has equal or superior authority to English text in interpreting Article II.').

omega_variable(
    kawanatanga_scope_ambiguity,
    'Does ''kāwanatanga'' (governorship) limited to Crown mean: (a) Crown governance only of settler populations, or (b) Crown governance of all populations but with Māori internal administration retained?',
    'Linguistic analysis of 1840 Māori language and usage; historical correspondence between Māori negotiators and Crown officials; Māori oral traditions about what was agreed.',
    'Reading (a) supports full Māori territorial sovereignty under rangatiratanga; reading (b) supports Crown supremacy with Māori delegated authority. The constraint''s classification hinges on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kawanatanga_scope_ambiguity, empirical, 'Scope of Crown kāwanatanga (governance) authority under Article II Māori text.').

omega_variable(
    theater_ratio_growth_driver,
    'Is the rising theater ratio (0.05 → 0.62) driven by genuine institutional shift toward recognizing rangatiratanga, or by performative response to Māori political pressure while maintaining Crown control?',
    'Comparative analysis of treaty settlement outcomes versus stated commitments; examination of whether co-management structures give Māori veto power or advisory-only role; tracking whether Crown statutory law maintains paramountcy or yields genuine shared authority.',
    'Genuine shift would suggest the constraint is transitioning from snare toward tangled_rope or rope. Performative shift suggests the constraint remains snare with increasing theatrical maintenance. This determines whether contemporary governance reforms represent real change or inertial performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_growth_driver, empirical, 'Whether rising theater ratio reflects genuine institutional change or performative suppression.').

omega_variable(
    suppression_internalization_vs_structural,
    'To what degree is Māori acceptance of Crown authority structural (legal barriers, enforcement machinery prevent exit) versus internalized (Māori have come to accept Crown supremacy as legitimate or inevitable)?',
    'Post-exit trajectory analysis: if Māori communities were freed from legal constraints and enforcement suppression, would they immediately reassert rangatiratanga or would internalized Crown authority persist? Comparative analysis of iwi that have negotiated greater autonomy (e.g., through settlement or iwi co-management) versus those under direct Crown administration.',
    'If suppression is primarily structural, removing Crown enforcement barriers would enable rangatiratanga reassertion. If suppression is internalized, the constraint would persist even with barriers removed. This determines the cost and difficulty of transitioning from Crown sovereignty to operationalized rangatiratanga.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression of rangatiratanga is structural or internalized.').

omega_variable(
    kernel_reading_coexistence,
    'Can the rangatiratanga reading and the crown_sovereignty_reading coexist in a single New Zealand constitutional framework, or do they logically foreclose each other?',
    'Jurisprudential analysis: examining whether courts have ever held both readings simultaneously or whether accepting one requires rejecting the other. Federal or consociational models that institutionalize dual sovereignty (e.g., the New Zealand Bill of Rights Act''s recognition of Treaty principles) may demonstrate coexistence possibility.',
    'If they can coexist, institutional innovation (dual governance, co-legislation) could accommodate both readings. If they foreclose each other, the constraint''s resolution requires choosing one reading and suppressing the other. This determines whether institutional reform can satisfy both claims or whether one claim must prevail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Whether rangatiratanga and crown_sovereignty readings are logically compatible.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__rangatiratanga_reading, 0, 180).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(wait_tr_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(wait_tr_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement(wait_tr_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 90, 0.45).
narrative_ontology:measurement(wait_tr_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 120, 0.58).
narrative_ontology:measurement(wait_tr_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 150, 0.62).
narrative_ontology:measurement(wait_tr_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, theater_ratio, 180, 0.62).

% Extraction over time
narrative_ontology:measurement(wait_be_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(wait_be_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(wait_be_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(wait_be_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 90, 0.64).
narrative_ontology:measurement(wait_be_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement(wait_be_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(wait_be_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, base_extractiveness, 180, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t0, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(wait_su_t30, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 30, 0.42).
narrative_ontology:measurement(wait_su_t60, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 60, 0.56).
narrative_ontology:measurement(wait_su_t90, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 90, 0.66).
narrative_ontology:measurement(wait_su_t120, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 120, 0.71).
narrative_ontology:measurement(wait_su_t150, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 150, 0.71).
narrative_ontology:measurement(wait_su_t180, waitangi_sovereignty_allocation__rangatiratanga_reading, suppression_requirement, 180, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__rangatiratanga_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(waitangi_sovereignty_allocation__rangatiratanga_reading, 0.12).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__rangatiratanga_reading, waitangi_sovereignty_allocation__partnership_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel waitangi_sovereignty_allocation. The rangatiratanga_reading asserts Māori retained inherent authority over territories and resources; it has ε=0.68 under this reading. The crown_sovereignty_reading interprets Article I as conveying complete sovereignty to Crown with Article II as subordinate reassurance; this is a separate constraint with different ε and different beneficiary/victim structure. The partnership_reading asserts an ongoing Crown-Māori partnership framework; this is a third constraint with a different structural relationship. All three are linked via network.affects_constraints because they are competing interpretations of the same Treaty kernel, and acceptance of one reading creates institutional pressure on the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__rangatiratanga_reading, organized, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
