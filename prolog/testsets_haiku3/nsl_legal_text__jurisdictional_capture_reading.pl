% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Jurisdictional Capture Vehicle
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   The National Security Law (NSL), implemented in Hong Kong in 2020, is
 *   contested as either a legitimate sovereignty instrument or a vehicle for
 *   mainland legal system transplantation that erodes HK's common-law
 *   autonomy. This constraint story instantiates the
 *   jurisdictional_capture_reading: the NSL functions as a mechanism through
 *   which mainland security apparatus captures control over HK's judiciary
 *   and legal profession, substituting mainland civil-law doctrines (unified
 *   authority, state prerogative) for common-law principles (judge-made law,
 *   separation of powers, individual rights). The reading does not claim the
 *   NSL lacks genuine security objectives; rather, it asserts that legal
 *   transplantation (mainland court review authority, subordination of HK
 *   judicial precedent) operates as an extractive overlay on those
 *   objectives—capturing judicial independence as a side effect or parallel
 *   benefit. The constraint is authored as tangled_rope because genuine
 *   coordination exists (unified national security framework) alongside
 *   asymmetric extraction (HK judicial autonomy → mainland security
 *   apparatus). The beneficiary is the mainland security apparatus (gains
 *   institutional control and removes a competing legal authority); the
 *   victims are HK judiciary and legal profession (lose jurisdictional
 *   autonomy, face suppression of resistance). The measurement series runs
 *   from 2020 (NSL implementation) through 2024 (interval end), tracking
 *   extractiveness and theater_ratio as they climbed, and
 *   suppression_requirement as it stabilized at a high plateau.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: Beneficiary (gains unified authority over national security determinations, removes HK judicial review capacity)
 *   - hong_kong_judiciary: Primary victim (loses jurisdictional autonomy over NSL-defined offenses, subject to mainland court review per Article 18)
 *   - legal_profession_hk: Primary victim (professional doctrine (common law) subordinated to mainland civil-law authority; career incentives shift toward mainland legal training)
 *   - hong_kong_civil_society: Affected party; faces narrowed legal defenses for political expression and organizing
 *   - beijing_leadership: Agenda setter (implements NSL, enforces Article 18 review authority)
 *   - international_legal_observers: Observer seat (documents compliance patterns, jurisdictional boundary disputes)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.67).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.71).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.67).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Jurisdictional Capture Vehicle").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '59669c98-20c2-4ab2-955b-bdcd8ebc5f07').
narrative_ontology:cs_kernel_codification('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', formalized).
narrative_ontology:cs_authority_grounding('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', extraction).
narrative_ontology:cs_interpretation_layer_present('59669c98-20c2-4ab2-955b-bdcd8ebc5f07').
narrative_ontology:cs_reading_relation('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', foundational, institutional_capture_via_doctrinal_substitution).
narrative_ontology:cs_axiom_status(institutional_capture_via_doctrinal_substitution, holdable).
narrative_ontology:cs_axiom_grounding('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', institutional_capture_via_doctrinal_substitution, empirically_contingent).
narrative_ontology:cs_axiom('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', foundational, common_law_separation_as_value).
narrative_ontology:cs_axiom_status(common_law_separation_as_value, holdable).
narrative_ontology:cs_axiom_grounding('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', common_law_separation_as_value, deontological).
narrative_ontology:cs_reference_frame('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', common_law_judicial_autonomy_framework).
narrative_ontology:cs_drift_state('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', contemporary_2024, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('59669c98-20c2-4ab2-955b-bdcd8ebc5f07', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, legal_profession_hk).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, beijing_leadership).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Beijing's national security, Ministry of State Security, and People's Armed Police exercise authority over NSL implementation, Article 18 review of HK court determinations, and enforcement of security definitions. They set the agenda by implementing the NSL statute, defining what constitutes national security offense, training mainland judges to apply the standard to HK cases, and managing professional integration (encouraging HK legal professionals to train in mainland doctrine). They directly collect the institutional benefit: centralized authority over security determinations, removal of HK judicial review as a constraint on mainland interests, and normalization of mainland legal doctrine in HK institutional practice.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% HK judges lose jurisdictional autonomy over NSL-defined offenses per Article 18: mainland courts review HK judicial determinations on national security matters, creating a two-tier review structure inconsistent with common-law judicial independence. Judges face institutional suppression (cases dismissed on Article 18 grounds, judicial decisions overruled by mainland review, professional discipline for NSL-inconsistent reasoning) and doctrinal subordination (common-law precedent loses precedential weight in security-related cases). Exit is identity-locked: judges trained in common-law tradition face professional identity rupture if they leave HK, and some leave the profession entirely rather than practice under NSL constraints. Their time horizon is biographical: careers planned around HK's common-law system are disrupted mid-career.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, biographical, identity_locked, regional).

% HK lawyers lose doctrinal autonomy as NSL provisions are integrated into practice. Professional doctrine (common law, adversarial procedure, client confidentiality within bounds) is subordinated to mainland civil-law standards (unified state authority, inquisitorial elements, state prerogative over professional privilege). Bar associations face institutional suppression: leadership is replaced or constrained, objections to NSL provisions are interpreted as political speech, professional standards that conflict with NSL are superseded. Career incentives shift: younger lawyers increasingly train in mainland doctrine to be employable; HK legal education curricula integrate mainland constitutional law. Exit is identity-locked: common-law lawyers who leave HK often de-professionalize rather than practice mainland law. Their time horizon is biographical: professional formations are disrupted in mid-career; younger practitioners adapt rather than resist.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, legal_profession_hk, payer,
    organized, biographical, identity_locked, regional).

% Beijing's political leadership (National People's Congress Standing Committee, Central Committee) enacts the NSL statute, interprets its scope, and controls enforcement priorities. They set the agenda for security doctrine and institutional integration. They benefit from unified authority (no competing jurisdictional claims to security determinations), removal of HK autonomy as a constraint on mainland interests, and normalization of mainland legal doctrine across all territories under PRC authority.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, beijing_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, beijing_leadership, beneficiary).

% Civil society faces narrowed legal defenses for political expression, organizing, and collective action. The NSL expands definitions of national security offense to include some forms of advocacy, political organizing, and international coordination on HK governance issues. Legal protections that common law provided (right to petition, political speech, assembly) are constrained by NSL interpretation. Exit is constrained: emigration is possible for those with resources and skills, but most civil society participants lack exit options and face increasing suppression. Their time horizon is biographical: organizing and advocacy work that was previously legal are now prosecutable.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society, payer,
    powerless, biographical, constrained, regional).

% The HK common-law tradition—a set of jurisprudential principles, judicial precedents, professional practices, and constitutional understandings inherited from British rule and developed through HK's judicial system—is the non-agent entity that bears extraction costs. This tradition is not an actor, but it is the referent against which HK judges and lawyers measure their institutional position: its subordination to mainland doctrine is experienced by legal professionals as the loss of something collectively valuable. The tradition persists in depoliticized legal domains but is increasingly displaced in security-related jurisprudence.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hk_common_law_tradition, payer,
    analytical, generational, analytical, regional).
narrative_ontology:stakeholder_non_agent(nsl_legal_text__jurisdictional_capture_reading, hk_common_law_tradition).

% International human rights organizations, foreign legal scholars, bar associations in common-law jurisdictions, and UN human rights mechanisms observe and document NSL implementation patterns. They provide independent corroboration of the jurisdictional_capture_reading (through reports on HK judicial independence decline, comparative legal analysis of civil-law vs. common-law authority structures, and testimony from HK legal professionals) and contestation of competing readings (sovereignty_restoration reading is contested through reports on overreach beyond security; democratic_enclosure reading is contested through civil-society testimony). They do not enforce or benefit directly from the constraint, but their observations feed the mandatrophy analysis and feed international responses (sanctions, statements, precedent in other jurisdictions).
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_legal_observers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified security framework: integration of HK's legal system into mainland national security doctrine eliminates fragmented authority over security determinations, creates consistent legal standards across territories under PRC authority, and coordinates response to cross-border security threats. Under the beneficiary seat's reading, this is a genuine coordination solution to a real problem (institutional instability during 2019 unrest, competing legal authorities on cross-border crimes). Under the victim seat's reading, this coordination is real but subordinated to an extractive overlay: the coordination could be achieved through preserved HK judicial autonomy (narrow definitional alignment without mainland court review); the mainland court review is an additional extraction mechanism, not a coordination necessity.
% TRANSFER_FUNCTION: Moves institutional control and jurisdictional authority from HK's independent judiciary to mainland security apparatus. Also moves legal doctrine from common-law tradition (judge-made law, precedent, adversarial procedure) to mainland civil-law tradition (unified state authority, written codes, inquisitorial elements). Professional identity and career incentives shift from HK common-law practice toward mainland civil-law training. Extractiveness is measured in jurisdictional autonomy lost (common-law courts lose review authority) and doctrine subordinated (common-law precedent loses precedential weight in security cases).
% ABSENT_VOICES: Voices structurally excluded: pre-NSL HK civil society and pro-democracy activists who would articulate objections are silenced by NSL enforcement (political speech criminalized, organizing prosecuted, leadership imprisoned). Voices geographically absent: international common-law legal traditions (UK, US, Commonwealth jurisdictions) that would advocate for judicial independence preservation are outside HK enforcement jurisdiction. These voices have been silenced or removed by the constraint's operation—their absence reflects suppression and emigration, not initial non-participation.
% DISAPPEARANCE_RATIONALE: If the NSL and Article 18 jurisdictional review vanished overnight, HK's legal system would experience rapid institutional reorganization: judges would resume common-law authority over security-related cases, HK Bar Association would reclaim doctrinal autonomy, legal education would revert to common-law emphasis, and civil society would regain legal defenses for political speech. Career incentives for HK lawyers would shift away from mainland training. International recognition of HK judicial independence would restore. This reorganization would occur within months because the institutional infrastructure (HK courts, Bar Association, legal education capacity) persists beneath NSL constraints; they are dormant, not destroyed. The world of Hong Kong legal practice and civil society organizing depends on this constraint's continuous operation—its removal would trigger immediate rearrangement.
% FOUNDING_PROBLEM: 2019 Hong Kong unrest created institutional instability and demonstrated competing authority claims: HK legal system (common law, judicial independence) and mainland authority (constitutional sovereignty, national security prerogative) produced divergent responses to protest movements, political organizing, and international coordination. Beijing asserts that HK's judicial independence and civil-law autonomy created space for anti-government organizing, violence during protests, and coordination with foreign actors opposed to PRC interests. This founding problem motivated the NSL as a legal vehicle to unify security authority and prevent institutional fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: Beijing attests the founding problem is live: ongoing threats of foreign interference (US support for HK activists), coordination with overseas anti-PRC organizations, and transnational organizing are cited as evidence that institutional separation between HK and mainland creates security vulnerabilities. HK legal professionals and international observers contest this: they attest that the founding problem (overt unrest) was substantially reduced by 2020 before NSL implementation (protest activity had declined, violence had subsided), and that legal autonomy is not the source of ongoing threat; they read the NSL as security theater addressing a problem already substantially mitigated. No corroboration exists from outside the mainland security apparatus for the claim that HK's judicial autonomy poses an active security threat requiring legal transplantation rather than narrower security coordination.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.67 reflects that the constraint captures a core institutional good (judicial independence) from HK's legal system to mainland security apparatus. The trajectory (0.38 → 0.67 over 20 years) shows accelerating capture as mainland courts formalize Article 18 review authority and HK legal education curricula incorporate mainland doctrine. Suppression is high (0.71) because HK judges and lawyers who resist NSL doctrine face immediate costs: case dismissals, professional discipline, career barriers; but suppression plateaus (not rising further by year 16) because alternative paths emerge (emigration, deprofessionalization, intellectual accommodation). Theater_ratio rises throughout (0.12 → 0.42) as NSL enforcement shifts: early period emphasized security prosecutions; by year 12, enforcement increasingly targets political speech and civil society organizing, expanding the function beyond narrow security into broader social control—the gap between security framing and actual enforcement activity widens, marking the transition from coordination to theater. The accessibility_collapse (0.58) reflects that alternatives to NSL authority are not completely closed (HK common-law practice persists in depoliticized domains, emigration remains an option, international legal standards remain formally recognized in HK's constitutional framework), but the collapse is substantial: the NSL's existence narrows the legal space for dissent and strips away defenses available under pure common law. Resistance (0.62) indicates active pushback: HK Bar Association objections, judicial dissents, civil society litigation, and international criticism mount steadily; the constraint does not operate against universal acquiescence—but resistance is insufficient to reverse the trajectory because the mainland security apparatus controls enforcement machinery and HK lacks independent enforcement power to contest it.
 *
 * PERSPECTIVAL GAP:
 *   The mainland security apparatus experiences the NSL as genuine coordination: unifying security doctrine, removing fragmented authority, establishing consistent frameworks across all territories. From this beneficiary seat, extractiveness appears as coordination cost (structural asymmetry necessary for sovereign authority to function). The HK judiciary and legal profession experience the NSL as institutional capture: common-law doctrines they trained in, legitimate precedents they relied on, and jurisdictional independence they inherited are subordinated to external authority without consent. From this victim seat, the same structure operates as extraction—the coordination benefits accrue almost entirely to mainland security apparatus; HK citizens and legal professionals bear the costs of doctrinal substitution, narrowed legal defenses, and professional identity rupture. International legal observers split: those rooted in common-law traditions read institutional capture; those rooted in civil-law traditions or focused on state sovereignty read legitimate authority assertion. The engine computes this perspectival divergence from the structural data: beneficiary seat (institutional power, extraction receiver) vs. victim seat (institutional dependence, extraction bearer) produces different per-seat type classifications even though the constraint is structurally identical.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is highly asymmetric: mainland_security_apparatus sits at d≈0.95 (full target of extraction benefits—institutional control and authority centralization flow to this seat with no countervailing cost), while hong_kong_judiciary and legal_profession_hk sit at d≈0.85–0.88 (full targets of extraction costs—jurisdictional autonomy lost, doctrine subordinated, resistance suppressed). The asymmetry drives the tangled_rope classification: genuine coordination (unified security framework) is bundled with pure extraction (institutional capture). If the constraint were purely rope (symmetric coordination), both seats would cluster near d≈0.5; if purely snare (pure extraction with coercion cover), both would cluster at d≈0.85–1.0 but the beneficiary seat would have lower power and mobility, giving it escape options. The mainland security apparatus has high institutional power, global scope, mobile exit (can redefine NSL scope, shift enforcement priorities), and beneficiary status—d is low (full beneficiary). HK judiciary has institutional power but is geographically trapped, identity-locked (professional identity fused with common-law practice), faces active suppression, and is in the victim role—d is high (full target). This asymmetry is structurally irreducible: it tracks the actual power imbalance and the actual distribution of costs and benefits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (2019 unrest, institutional instability, security threats to mainland-HK integration) is live from the beneficiary seat's perspective and contested from the victim seat's perspective. At the time of NSL implementation (2020), the founding problem appeared live to Beijing (unrest had not subsided, institutional autonomy seemed incompatible with order). By 2024, the founding problem status is contested: security metrics show reduced overt protest activity (beneficiary reading: problem solved), but civil society reports show deeper suppression, depoliticization, and emigration (victim reading: problem was never the real target; social control was). Theater_ratio rising from 0.12 to 0.42 indicates mandate drift: the NSL's published mandate (security against foreign interference, maintenance of stability) has not been formally revised, but enforcement patterns reveal a mission creep into political speech, civil organizing, and ideological conformity—the gap between published mandate and actual enforcement is the mandatrophy signal. The constraint has not resolved its mandate (founding problem remains live and contested), but the functional enforcement has drifted toward social control theater. This is the exact profile that mandatrophy_analysis documents: a coordination function (security) parasitized by an extraction function (institutional capture and political control). The classification tangled_rope holds because the coordination component is real (security cooperation problems are addressed), but the extraction component is at least as large and growing (measured by theater_ratio climb and accessibility_collapse persistence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_legitimacy,
    'Is the NSL fundamentally a jurisdictional capture mechanism eroding common-law autonomy, or is it a legitimate sovereign security instrument restoring constitutional order?',
    'This omega names the kernel contest itself: the jurisdictional_capture_reading and the sovereignty_restoration_reading hold incompatible core premises about the NSL''s foundational legitimacy. Resolution would require a framework that adjudicates which reading''s core claim (capture vs. restoration) is structurally true — but no such framework exists neutral to both parties. The contest remains live.',
    'If capture is the correct reading, the constraint''s beneficiary (mainland security) and victim (HK judicial autonomy) structure holds, extractiveness is moderate-high, and classification tracks tangled_rope or snare. If sovereignty-restoration is correct, beneficiary/victim structure reverses, extractiveness drops, and the reading reclassifies as rope or scaffold. The classification depends entirely on which reading''s legitimacy claim is accepted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_legitimacy, conceptual, 'Kernel reading legitimacy: is NSL fundamentally capture or legitimate sovereign restoration?').

omega_variable(
    jurisdictional_boundary_interpretation,
    'Does Article 18 of the NSL (restricting HK courts'' jurisdiction over offenses with national security dimensions) represent a legitimate delineation of sovereignty or an illegitimate subordination of HK''s judicial independence?',
    'Empirical: documented patterns of mainland courts reviewing HK judicial determinations; HK courts'' rates of dismissing cases on Article 18 grounds; comparison with pre-NSL judicial independence indicators. Conceptual: whether the mainland''s constitutional law tradition (civil law, unified authority) is compatible with HK''s common-law tradition (judge-made law, separation of powers) when both claim authority over the same acts.',
    'If Article 18 constitutes true jurisdictional capture (mainland court review of HK judicial work), the victim set expands and extractiveness rises. If it is a legitimate sovereign line (security matters belong to Beijing), the constraint reclassifies toward rope or scaffold. The reading''s own authority grounding (lineage: inherited common-law jurisdictional autonomy) will collapse if the boundary interpretation shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(jurisdictional_boundary_interpretation, empirical, 'Whether NSL''s jurisdictional boundary represents capture or legitimate sovereignty.').

omega_variable(
    institutional_identity_fusion,
    'Have HK legal professionals fused their professional identity with common-law doctrine such that accepting NSL provisions feels like professional self-negation, independent of the constraint''s objective scope?',
    'Post-exit survey of legal professionals who left HK post-NSL: do those who fled continue asserting common-law framing, or have they adopted alternative frameworks? This distinguishes structural suppression (legal barriers to exit, economic cost) from identity-locked suppression (professional identity inseparable from common-law practice).',
    'If identity-locked, suppression of HK legal resistance persists even after legal/economic barriers are removed — victims carry the constraint with them. If structural only, exit removes suppression. This informs whether the measured suppression (0.71) reflects external barriers or internalized professional doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_identity_fusion, empirical, 'Professional identity fusion with common-law doctrine as suppression mechanism.').

omega_variable(
    common_law_transplantation_necessity,
    'Is mainland legal system transplantation structurally necessary to achieve the NSL''s stated national security objectives, or is the transplantation an independent extraction mechanism parasitic on the security frame?',
    'Counterfactual: could the same security objectives be achieved while preserving HK''s common-law jurisdictional autonomy (e.g., narrow definitional alignment without mainland court review)? Comparative: do other jurisdictions with dual legal systems (e.g., EU member states with separate legal traditions) achieve comparable security coordination without jurisdictional capture?',
    'If transplantation is necessary, part of extractiveness is coordination cost (tangled_rope). If parasitic, the transplantation component is pure extraction (snare), and the coordination component is separable. This determines whether the constraint''s type derives from genuine coordination with asymmetric extraction (tangled_rope) or pure extraction hiding under a security frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_transplantation_necessity, conceptual, 'Whether legal transplantation is necessary for security or is an independent extraction mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(nsl__tr_t4, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 4, 0.2).
narrative_ontology:measurement(nsl__tr_t8, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 12, 0.35).
narrative_ontology:measurement(nsl__tr_t16, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 16, 0.4).
narrative_ontology:measurement(nsl__tr_t20, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 20, 0.42).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nsl__be_t4, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(nsl__be_t8, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 8, 0.56).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(nsl__be_t16, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 16, 0.66).
narrative_ontology:measurement(nsl__be_t20, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 20, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(nsl__su_t4, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 4, 0.59).
narrative_ontology:measurement(nsl__su_t8, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 8, 0.64).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 12, 0.68).
narrative_ontology:measurement(nsl__su_t16, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 16, 0.71).
narrative_ontology:measurement(nsl__su_t20, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 20, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__jurisdictional_capture_reading, 0.18).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% The nsl_legal_text kernel spawns three distinct constraint stories corresponding to three distinct readings. The jurisdictional_capture_reading (this story) focuses on institutional structure (judicial autonomy erosion via legal system transplantation). The sovereignty_restoration_reading presents the NSL as legitimate constitutional assertion and yields different extractiveness (lower), different beneficiary/victim structure (mainland public order vs. HK pro-independence activists), and different type (rope or scaffold rather than tangled_rope). The democratic_enclosure_reading focuses on political space closure (criminalizes speech and organizing) and yields a different victim set (civil society, journalists) and higher extractiveness (captures political freedom). All three readings share the same kernel (the NSL statute) but instantiate different constraints with different ε values, different stakeholder structures, and different classifications. They are linked via network.affects_constraints; the relationship is coexistence—each reading remains a live position held by different institutional and civil-society actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
