% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__sovereignty_restoration_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-14
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__sovereignty_restoration_reading, []).

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
 *   constraint_id: nsl_legal_text__sovereignty_restoration_reading
 *   human_readable: National Security Law (Sovereignty Restoration Reading)
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   The National Security Law (NSL) for Hong Kong, promulgated June 30, 2020,
 *   is read here as a legitimate sovereign instrument restoring
 *   constitutional order after the 2019 anti-extradition bill protests
 *   escalated into widespread unrest, legislative paralysis, and what the
 *   central government characterizes as a 'color revolution' attempt. This
 *   reading holds that the NSL's four core offenses (secession, subversion,
 *   terrorism, collusion with foreign forces) target a narrow band of actors
 *   threatening state sovereignty, while the law's institutional mechanisms
 *   (Committee for Safeguarding National Security, Office for Safeguarding
 *   National Security, designated judges) provide necessary coordination to
 *   prevent governance collapse. The constraint extracts from political
 *   opposition forces (pro-democracy activists, opposition legislators,
 *   independent media, civil society organizations) who are structurally
 *   positioned as security threats under this reading, while benefiting the
 *   central government authority, HKSAR government, pro-establishment
 *   legislators, and the national security apparatus. The coordination
 *   function is real — the 2019 unrest created a genuine governance vacuum —
 *   but the extraction is asymmetric: political opposition bears costs
 *   disproportionate to their threat level, and the security apparatus gains
 *   institutional expansion.
 *
 * KEY AGENTS:
 *   - central_government_authority: Primary beneficiary (institutional/arbitrage) — asserts sovereign prerogative, defines security threats
 *   - hk_sar_government: Beneficiary (institutional/constrained) — restored executive capacity, but autonomy reduced by Beijing's direct mechanisms
 *   - pro_establishment_legislators: Beneficiary (organized/constrained) — legislative control restored, but operate within Beijing-defined boundaries
 *   - national_security_apparatus: Beneficiary (institutional/identity_locked) — new institutional powers, budget, status; career paths fused to NSL enforcement
 *   - pro_democracy_activists: Victim (powerless/trapped) — facing severe charges, exile, or self-censorship; exit options collapsed
 *   - opposition_legislators: Victim (moderate/trapped) — disqualified, arrested, or forced out; legislative exit blocked
 *   - independent_media_outlets: Victim (organized/constrained) — forced closure (Apple Daily, Stand News), chilling effects on survivors
 *   - civil_society_organizations: Victim (organized/trapped) — disbanded (Hong Kong Alliance, Professional Teachers' Union), registration pressures
 *   - foreign_governments: Observer (institutional/analytical) — sanction responses, consular protections, diplomatic pressure
 *   - legal_profession: Observer (organized/analytical) — designated judges system, common law tradition under pressure, professional ethics tensions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, 0.48).
domain_priors:suppression_score(nsl_legal_text__sovereignty_restoration_reading, 0.52).
domain_priors:theater_ratio(nsl_legal_text__sovereignty_restoration_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(nsl_legal_text__sovereignty_restoration_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__sovereignty_restoration_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__sovereignty_restoration_reading, "National Security Law (Sovereignty Restoration Reading)").
narrative_ontology:topic_domain(nsl_legal_text__sovereignty_restoration_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__sovereignty_restoration_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__sovereignty_restoration_reading, '0b777345-8270-4ed2-bed5-bae45472682c').
narrative_ontology:cs_kernel_codification('0b777345-8270-4ed2-bed5-bae45472682c', formalized).
narrative_ontology:cs_authority_grounding('0b777345-8270-4ed2-bed5-bae45472682c', lineage).
narrative_ontology:cs_interpretation_layer_present('0b777345-8270-4ed2-bed5-bae45472682c').
narrative_ontology:cs_reading_relation('0b777345-8270-4ed2-bed5-bae45472682c', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('0b777345-8270-4ed2-bed5-bae45472682c', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('0b777345-8270-4ed2-bed5-bae45472682c', foundational, sovereign_security_prerogative_absolute).
narrative_ontology:cs_axiom_status(sovereign_security_prerogative_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0b777345-8270-4ed2-bed5-bae45472682c', sovereign_security_prerogative_absolute, conventional).
narrative_ontology:cs_axiom('0b777345-8270-4ed2-bed5-bae45472682c', foundational, protest_as_security_threat_legitimate).
narrative_ontology:cs_axiom_status(protest_as_security_threat_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('0b777345-8270-4ed2-bed5-bae45472682c', protest_as_security_threat_legitimate, conventional).
narrative_ontology:cs_reference_frame('0b777345-8270-4ed2-bed5-bae45472682c', pre_nsl_governance_vacuum).
narrative_ontology:cs_drift_state('0b777345-8270-4ed2-bed5-bae45472682c', post_nsl_implementation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0b777345-8270-4ed2-bed5-bae45472682c', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, central_government_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, hk_sar_government).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_legislators).
narrative_ontology:constraint_beneficiary(nsl_legal_text__sovereignty_restoration_reading, national_security_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, independent_media_outlets).
narrative_ontology:constraint_victim(nsl_legal_text__sovereignty_restoration_reading, civil_society_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts sovereign prerogative to define national security threats and enforcement mechanisms for Hong Kong. The NSL enables direct central intervention in HK affairs (Article 55, Office for Safeguarding National Security) without HKSAR legislative approval. Collects institutional power: appointment/removal of HKSAR officials, interpretation authority, override of local judicial decisions. No extraction from this constraint — it is the author and ultimate enforcer.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, central_government_authority, beneficiary,
    institutional, generational, arbitrage, global).

% Restored executive capacity after 2019 paralysis: policy implementation resumed, legislative gridlock broken, civil service discipline restored. But autonomy constrained: Article 23 local legislation shelved indefinitely, Beijing's direct appointment powers (Secretary for Justice, Chief Secretary) bypass HKSAR consultation, NSL Committee chaired by CE but subject to central oversight. Benefits from order restoration but loses constitutional space.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, hk_sar_government, beneficiary,
    institutional, biographical, constrained, national).

% Legislative control restored after 2019 opposition disruption (filibustering, oath-taking controversies). Elects CE and passes laws within Beijing-defined boundaries (patriots-only electoral system post-2021). Gains stable legislative career but operates within narrowing political space — dissent from Beijing's line risks deselection. Exit constrained: party discipline and Beijing liaison office oversight.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_establishment_legislators, beneficiary,
    organized, biographical, constrained, national).

% New institutional powers created by NSL: Committee for Safeguarding National Security (Art 12), Office for Safeguarding National Security (Art 48), dedicated national security police division, designated judges pool. Budget and staffing expanded rapidly. Career paths now fused to NSL enforcement — promotion depends on national security credentials. Identity_locked: professional self-concept constituted through 'safeguarding sovereignty' mission. Could not exit without professional identity dissolution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, national_security_apparatus, beneficiary,
    institutional, generational, identity_locked, national).

% Facing severe charges under Articles 20-29 (subversion, secession, collusion). 47 democrats charged in primary election case; Joshua Wong, Agnes Chow, Ivan Lam imprisoned; dozens fled into exile. Exit options collapsed: protest criminalized, electoral path blocked (disqualification), speech chilled. Trapped: physical exit possible (exile) but political identity destroyed; remaining means self-censorship or imprisonment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, pro_democracy_activists, payer,
    powerless, biographical, trapped, national).

% Mass resignation 2020 after disqualification of four colleagues; subsequent disqualification of remaining opposition figures. Legislative exit blocked — no pathway to return under patriots-only system. Some imprisoned (Lam Cheuk-ting, Ted Hui in exile). Trapped: political career destroyed, legal jeopardy ongoing, no institutional avenue for dissent.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, opposition_legislators, payer,
    moderate, biographical, trapped, national).

% Apple Daily forced closure June 2021 (asset freeze, arrests of executives); Stand News closed Dec 2021 (sedition charges); Citizen News, Mad Dog Daily voluntarily ceased operations. Surviving outlets (HKFP, The Witness) operate under severe chilling effects — self-censorship on national security topics, funding pressures. Exit constrained: can cease operations or relocate offshore, but Hong Kong audience access blocked or degraded.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, independent_media_outlets, payer,
    organized, biographical, constrained, national).

% Major organizations disbanded: Hong Kong Alliance (Tiananvigil), Professional Teachers' Union, Civil Human Rights Front, Hong Kong Confederation of Trade Unions. Registration ordinance amendments require national security vetting. Remaining groups face funding freezes, bank account closures, premises evictions. Trapped: organizational form cannot survive; individual members face personal liability.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, civil_society_organizations, payer,
    organized, biographical, trapped, national).

% Imposed sanctions (US Hong Kong Autonomy Act, UK Magnitsky-style sanctions, EU statements); suspended extradition treaties; offered BN(O) visa pathways (UK), residency pathways (Canada, Australia). Consular protections for dual nationals tested. Diplomatic pressure applied but no enforcement leverage over NSL itself. Analytical seat: monitors, documents, responds, but cannot alter constraint directly.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, foreign_governments, observer,
    institutional, generational, analytical, global).

% Designated judges system (Art 44) creates two-track judiciary; common law tradition under pressure (jury trials denied in NSL cases, mainland law interpretation). Law Society and Bar Association statements critical but muted. Lawyers representing NSL defendants face professional risk (Jimmy Lai's counsel). Exit analytical: can observe, critique, advocate internationally, but professional practice in HK constrained by loyalty expectations.
narrative_ontology:constraint_stakeholder(nsl_legal_text__sovereignty_restoration_reading, legal_profession, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__sovereignty_restoration_reading, national_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__sovereignty_restoration_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Restored executive-legislative functionality after 2019 paralysis: policy implementation resumed, budget passed, civil service discipline restored, protest-related disruption ended. Provided legal basis for national security institutions that previously had no HK statutory footing.
% TRANSFER_FUNCTION: Moves political space, organizational survival, and personal liberty from pro-democracy opposition (activists, legislators, media, civil society) to the national security apparatus (new powers, budget, status) and central authority (direct governance mechanisms). The transfer is not primarily monetary but institutional and existential.
% ABSENT_VOICES: Hong Kong general population (especially youth) — would object to reduced political participation and civil liberties but are not represented in the NSL's drafting or implementation. Taiwan — directly affected by 'one country two systems' credibility collapse but excluded from any consultation. UN human rights mechanisms — consistently critical but denied access. These voices are structurally excluded by the constraint's sovereignty frame.
% DISAPPEARANCE_RATIONALE: If NSL vanished overnight: national security apparatus loses statutory basis; Office for Safeguarding National Security loses legal footing; designated judges system dissolves; opposition figures could return from exile; independent media could restart; civil society could reorganize; Article 23 local legislation would become urgent; Hong Kong's political system would revert to pre-2020 contestation. The world rearranges fundamentally.
% FOUNDING_PROBLEM: 2019 anti-extradition bill protests escalated into 7 months of widespread unrest: legislative siege, airport occupation, university sieges, 10,000+ arrests, governance paralysis (policy address delayed, budget scrutiny blocked), and what central authorities characterized as foreign-backed 'color revolution' attempt threatening sovereign authority.
% FOUNDING_PROBLEM_CORROBORATION: Beijing white papers (2020, 2022) and HKSAR policy addresses attest the problem persists: 'national security threats remain,' 'external forces continue interference.' UN Human Rights Committee (2022), US State Department reports, UK six-monthly reports, and Hong Kong legal scholars (Benny Tai, Johannes Chan, Margaret Ng) attest the original unrest was resolved by 2020 and the NSL now addresses a different problem: political opposition itself. Corroboration is split along sovereignty vs. rights frames.
narrative_ontology:disappearance_verdict(nsl_legal_text__sovereignty_restoration_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__sovereignty_restoration_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__sovereignty_restoration_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(nsl_legal_text__sovereignty_restoration_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__sovereignty_restoration_reading, 0.48, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__sovereignty_restoration_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__sovereignty_restoration_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) reflects moderate but targeted extraction: political opposition bears concentrated costs (arrests, disqualifications, closures, exile) while general population experiences chilling effects but not direct extraction. Suppression (0.52) is substantial but not total: opposition spaces are severely constrained but not eliminated (some dissent persists in coded forms, courts retain limited review). Theater ratio (0.28) indicates real coordination function (governance restoration) with growing performative layer (patriotic education, loyalty oaths, security theater). Accessibility collapse (0.42) shows alternatives partially closed: electoral path blocked, protest path criminalized, but discursive and cultural resistance persists. Resistance (0.58) remains significant: international pressure, legal challenges, civil society adaptation, and persistent public sentiment. The claimed_type 'tangled_rope' captures the genuine coordination (restoring governance after 2019 paralysis) fused with asymmetric extraction (targeting political opposition beyond security necessity).
 *
 * PERSPECTIVAL GAP:
 *   From the sovereignty restoration seat, the NSL is a necessary coordination mechanism — the 2019 unrest created a genuine governance vacuum that only sovereign intervention could fill. From the democratic enclosure seat (sibling reading), the same structure is a snare permanently closing political space. From the jurisdictional capture seat, it is a vehicle for mainland legal transplantation. The engine computes these divergences from the structural data: same constraint, different beneficiary/victim sets, different exit options, different directionalities. The authored claim (tangled_rope) and metrics are independent — the metrics describe the constraint's actual operation from this reading's lights.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government authority and national security apparatus are full beneficiaries (d ≈ 0.1-0.2): they define the constraint, collect institutional power, face no extraction. HKSAR government and pro-establishment legislators are partial beneficiaries (d ≈ 0.3): they gain governance capacity but lose autonomy to Beijing's direct mechanisms. Political opposition (activists, opposition legislators) are full targets (d ≈ 0.8-0.9): identity_locked/trapped exit, bear concentrated extraction. Independent media and civil society are high targets (d ≈ 0.7): constrained exit (some exile, some closure), bear extraction via forced dissolution. Foreign governments and legal profession are observers (d ≈ 0.5): analytical distance, no direct extraction or benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (2019 governance paralysis) is rated 'contested' — Beijing and pro-establishment claim it persists (ongoing security threats); opposition and international observers claim it was resolved by the NSL itself and now the constraint persists as extraction. Corroboration is split: Beijing's white papers and HKSAR policy addresses attest continuity; UN human rights reports, foreign government assessments, and local legal scholars attest the problem has shifted from 'unrest' to 'overbroad security law'. The constraint shows mandatrophy signals: theater_ratio rising (0.15→0.28) as performative loyalty rituals expand beyond security necessity; suppression_requirement stable at 0.52 despite declining protest activity, suggesting enforcement self-perpetuation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine reading of the NSL legal text kernel, or does the sovereignty restoration frame mask extractive political consolidation?',
    'Compare enforcement patterns against the textual scope of Articles 20-29 (secession, subversion, terrorism, collusion). If enforcement systematically exceeds textual boundaries toward political opposition, the reading is a cover. If enforcement tracks textual boundaries, the reading is structurally coherent.',
    'If cover: reclassify toward snare with higher extractiveness. If coherent: tangled_rope with moderate extractiveness stands. The sibling readings (democratic_enclosure_reading, jurisdictional_capture_reading) offer competing structural interpretations of the same kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the sovereignty restoration reading authentically captures the NSL''s operational logic or serves as legitimizing cover for democratic enclosure.').

omega_variable(
    extractiveness_distribution,
    'Is extractiveness concentrated on political opposition (moderate, targeted) or does it radiate to general population through chilling effects (higher, diffuse)?',
    'Measure self-censorship rates in general population vs. political actors; track Article 38 extraterritorial application frequency; monitor national security education permeation in schools and civil service.',
    'If diffuse: extractiveness rises toward 0.65-0.70, pushing toward snare classification. If targeted: moderate extractiveness (0.45-0.50) supports tangled_rope with genuine coordination function (security restoration) alongside asymmetric extraction (political opposition targeting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_distribution, empirical, 'Whether the NSL''s extraction targets a narrow political segment or spreads broadly through society.').

omega_variable(
    coordination_function_genuineness,
    'Does the NSL solve a genuine coordination problem (2019 unrest''s paralysis of governance) or is ''restoration'' a post-hoc justification for pre-planned centralization?',
    'Compare pre-NSL governance dysfunction metrics (legislative paralysis duration, policy implementation failure rate, protest-related economic disruption) against post-NSL restoration metrics. Cross-reference with Beijing''s pre-2019 policy documents on HK integration.',
    'If genuine coordination: tangled_rope holds with real rope component. If pretextual: the coordination story collapses, reclassifying toward snare with theater_ratio rising as ''restoration'' becomes performative.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_function_genuineness, conceptual, 'Whether the claimed coordination function (restoring order after 2019) is structurally real or a legitimating narrative.').

omega_variable(
    cp_government_beneficiary_capture,
    'Does the CPG/HKSAR government genuinely benefit from restored order, or has the national security apparatus captured the constraint to expand its own institutional power and budget?',
    'Track national security bureau budget growth, staffing expansion, and mission creep beyond Articles 20-29. Compare HKSAR government policy autonomy pre- and post-NSL (e.g., Article 23 local legislation shelving, Beijing''s direct appointment powers).',
    'If captured: beneficiary set shifts from ''sovereign authority'' to ''security apparatus'', changing directionality logic. The constraint becomes self-perpetuating bureaucratic extraction rather than sovereign coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cp_government_beneficiary_capture, empirical, 'Whether the declared beneficiaries (CPG/HKSAR) are the actual gain-capturers or whether the security bureaucracy has captured the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__sovereignty_restoration_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 6, 0.22).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(nsl__tr_t18, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__sovereignty_restoration_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 12, 0.48).
narrative_ontology:measurement(nsl__be_t18, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 18, 0.48).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__sovereignty_restoration_reading, base_extractiveness, 24, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 6, 0.52).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(nsl__su_t18, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 18, 0.52).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__sovereignty_restoration_reading, suppression_requirement, 24, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__sovereignty_restoration_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__sovereignty_restoration_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, article_23_local_legislation).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, electoral_system_reform_2021).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, patriotic_education_implementation).
narrative_ontology:affects_constraint(nsl_legal_text__sovereignty_restoration_reading, national_security_education_curriculum).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NSL legal text kernel. The three readings form a constraint family with distinct ε values, beneficiary/victim sets, and classifications, linked by network.affects_constraints. The sovereignty_restoration_reading claims moderate extractiveness (0.48) with genuine coordination; democratic_enclosure_reading would claim high extractiveness (>0.65) with no coordination; jurisdictional_capture_reading would claim moderate extractiveness (~0.50) with different coordination/extraction boundary (common law autonomy vs. mainland legal integration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, institutional, 0.15).
constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, organized, 0.75).
constraint_indexing:directionality_override(nsl_legal_text__sovereignty_restoration_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
