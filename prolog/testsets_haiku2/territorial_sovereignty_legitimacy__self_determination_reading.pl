% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__self_determination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__self_determination_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__self_determination_reading
 *   human_readable: Self-Determination Principle Applied to Arab Palestinian Population (Modern Legitimacy Reading)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested kernel
 *   'territorial_sovereignty_legitimacy'—the self-determination reading. It
 *   applies the modern principle of self-determination to the Arab
 *   Palestinian population, grounding legitimacy in demographic majority and
 *   continuous residence during the 19th-20th centuries (the 'modern
 *   period'). This reading frames the partition of 1948 and subsequent
 *   territorial arrangements as unjust impositions by external colonial and
 *   imperial powers, and frames Israeli state sovereignty as a colonial
 *   project contrary to self-determination principles. The constraint does
 *   NOT adjudicate whether this reading is correct—it models what the
 *   constraint's operation looks like when THIS reading is instantiated as a
 *   legitimacy framework. The measurement series tracks how extractiveness,
 *   suppression, and theater have evolved as the reading gained articulation
 *   (19th century nascence) through UN recognition (20th century
 *   codification) to contemporary contestation (21st century
 *   non-enforcement). The core extraction: the principle is invoked
 *   universally but applied selectively; it legitimates Palestinian claims
 *   normatively while external powers enforce non-application materially.
 *
 * KEY AGENTS:
 *   - Arab Palestinian population: the beneficiary under this reading; demographic majority with continuous residence justifies their sovereignty claim
 *   - Palestinian refugees/diaspora: the primary payers; their displacement is framed as illegitimate by this reading but remains unenforced, extraction persists
 *   - Occupied territories residents: dual-positioned; benefit from legitimacy claim but pay through non-enforcement and ongoing occupation
 *   - Arab nationalist movements: agenda-setters articulating and defending the self-determination principle in diplomatic and legal forums
 *   - External powers: institutional agenda-setters that pronounce the principle universally while selectively enforcing non-application to this case
 *   - Competing readings (covenant-continuity, existential-matrix): structurally excluded from adjudicating legitimacy under this framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, 0.82).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__self_determination_reading, 0.79).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__self_determination_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__self_determination_reading, resistance, 0.84).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__self_determination_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__self_determination_reading, "Self-Determination Principle Applied to Arab Palestinian Population (Modern Legitimacy Reading)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__self_determination_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__self_determination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__self_determination_reading, '692b4e88-14a0-4e99-b5fa-b3247bc1c593').
narrative_ontology:cs_kernel_codification('692b4e88-14a0-4e99-b5fa-b3247bc1c593', formalized).
narrative_ontology:cs_authority_grounding('692b4e88-14a0-4e99-b5fa-b3247bc1c593', extraction).
narrative_ontology:cs_interpretation_layer_present('692b4e88-14a0-4e99-b5fa-b3247bc1c593').
narrative_ontology:cs_reading_relation('692b4e88-14a0-4e99-b5fa-b3247bc1c593', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('692b4e88-14a0-4e99-b5fa-b3247bc1c593', territorial_sovereignty_legitimacy__existential_matrix_reading, coexists_with).
narrative_ontology:cs_axiom('692b4e88-14a0-4e99-b5fa-b3247bc1c593', foundational, modern_period_primacy).
narrative_ontology:cs_axiom_status(modern_period_primacy, holdable).
narrative_ontology:cs_axiom_grounding('692b4e88-14a0-4e99-b5fa-b3247bc1c593', modern_period_primacy, conventional).
narrative_ontology:cs_axiom('692b4e88-14a0-4e99-b5fa-b3247bc1c593', foundational, demographic_majority_legitimacy).
narrative_ontology:cs_axiom_status(demographic_majority_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('692b4e88-14a0-4e99-b5fa-b3247bc1c593', demographic_majority_legitimacy, empirically_contingent).
narrative_ontology:cs_axiom('692b4e88-14a0-4e99-b5fa-b3247bc1c593', secondary, external_power_imposition_is_unjust).
narrative_ontology:cs_axiom_status(external_power_imposition_is_unjust, holdable).
narrative_ontology:cs_axiom_grounding('692b4e88-14a0-4e99-b5fa-b3247bc1c593', external_power_imposition_is_unjust, deontological).
narrative_ontology:cs_reference_frame('692b4e88-14a0-4e99-b5fa-b3247bc1c593', arab_self_determination_recognized).
narrative_ontology:cs_drift_state('692b4e88-14a0-4e99-b5fa-b3247bc1c593', contemporary_non_enforcement_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('692b4e88-14a0-4e99-b5fa-b3247bc1c593', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_palestinian_population).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_diaspora).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__self_determination_reading, occupied_territories_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, occupied_territories_residents).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__self_determination_reading, arab_nationalist_movements).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, modern_self_determination_doctrine).
narrative_ontology:constraint_vindicates(territorial_sovereignty_legitimacy__self_determination_reading, demographic_legitimacy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The majority population of the territory during the modern period (19th-20th centuries) with continuous demographic presence and historical residence. This reading vindicates their claim to sovereignty through the modern principle of self-determination. Their benefit is the legitimacy claim itself—the constraint recognizes their right to governance and self-rule based on demographic majority and continuous occupancy. However, this benefit is contested and largely unenforced; the actual exercise of sovereignty remains constrained.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_palestinian_population, beneficiary,
    organized, generational, constrained, continental).

% Displaced populations who lost territorial claims and property rights through wars and displacement. Under this reading, they bear the cost of a legitimacy claim that does not restore their material status or rights of return in practice. The constraint frames their displacement as illegitimate (a result of external power imposition) but does not resolve it. They are bearers of extraction: their losses remain uncompensated and their displacement is maintained by external enforcement.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, palestinian_refugees_diaspora, payer,
    powerless, generational, trapped, global).

% Palestinian residents of occupied territories who benefit from the legitimacy principle (it supports their governance claims) but pay through de facto non-enforcement and ongoing military occupation. Their situation is ambiguous: they are simultaneously beneficiaries of the constraint's normative vindication and victims of its practical non-realization. Their exit options are severely constrained by occupation, restrictions on movement, and lack of functional state apparatus. Identity is substantially fused with Palestinian national identity, which grounds itself in the self-determination principle.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, occupied_territories_residents, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, occupied_territories_residents, beneficiary).

% Political and social movements that articulate, defend, and attempt to operationalize the self-determination principle. They set the agenda through diplomatic, legal, and institutional channels (UN declarations, Palestinian Authority positions, civil society advocacy). They benefit from the legitimacy the principle confers but bear the extraction cost of maintaining the claim under conditions of non-enforcement and international contestation.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, arab_nationalist_movements, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__self_determination_reading, arab_nationalist_movements, beneficiary).

% International actors (UN bodies, nation-states, imperial powers) that both recognize the self-determination principle in law and enforce its non-application through military support, diplomatic non-recognition of Palestinian sovereignty, and partition enforcement. They articulate the principle universally while selectively enforcing or suppressing its application to this specific case. This contradiction is the core extraction mechanism. Their mobility means they could shift to enforcement or alternative frameworks, but strategic interests maintain the status quo.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, external_powers, agenda_setter,
    institutional, generational, mobile, global).

% Proponents of covenant-continuity and existential-matrix readings are structurally excluded from adjudicating legitimacy under this framework. Their alternative historical narratives and legitimacy claims are not admitted as competing foundations. This reading's temporal scope (modern period) and its demographic primacy criterion foreclose rival temporal framings (ancient covenant) and rival legitimacy bases (existential rather than juridical).
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, competing_historical_readings, excluded,
    institutional, generational, constrained, global).

% Legal scholars, human rights bodies, and UN mechanisms that interpret and apply the self-determination principle. They witness and sometimes document the gap between the principle's universal statement and its selective enforcement in this territory. Their observational role creates a standing contestation: the principle's legitimacy rests on universality, but its application is particular and contested.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__self_determination_reading, international_law_community, observer,
    institutional, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__self_determination_reading, diffuse).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__self_determination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The self-determination principle solves a coordination problem at the foundation of modern statehood: how to determine legitimacy of governance claims in a world of nation-states. By anchoring legitimacy to the demonstrated will of a people living in a territory over an identifiable modern period, the principle creates a transparent, universalizable standard for state formation and territorial claims.
% TRANSFER_FUNCTION: The constraint moves legitimacy recognition and governance authority FROM external powers and competing historical claims TO the Arab Palestinian population understood as a nation exercising self-determination. It transfers sovereignty claims from the colonial administrations, imperial powers, and Jewish immigration schemes that shaped the territory to a framework centered on indigenous demographic majority and continuous residence. In practice, however, this transfer is asserted but not enforced; external powers selectively apply the principle while maintaining occupation and partition.
% ABSENT_VOICES: Proponents of the covenant-continuity reading (grounding legitimacy in ancient Jewish presence and divine covenant) and existential-matrix reading (grounding legitimacy in collective survival imperatives rather than juridical demographics) are structurally excluded from this reading's adjudication framework. Their territorial claims are not admitted as competing legitimate bases under self-determination logic. Additionally, Jewish immigration movements and their international supporters are excluded from determining the temporal scope—this reading fixes the 'modern period' as 19th-20th centuries, foreclosing ancient-history and pre-modern-immigration arguments.
% DISAPPEARANCE_RATIONALE: If this constraint vanished—if the self-determination principle were abandoned as a legitimacy foundation—the territorial order would reorganize. Palestinian governance claims would lose their primary modern legal-philosophical grounding; partition arrangements would require alternative justifications (pure power, existential-matrix logic, covenant claims); international law would fragment further on the legitimacy question; diplomatic frameworks built on UN resolutions invoking self-determination would collapse or require restatement. The world does not rearrange randomly, but the specific state-architecture and the distribution of recognition/non-recognition flows through this principle's application.
% FOUNDING_PROBLEM: The founding problem was: how should sovereignty legitimacy be determined in a territory with competing historical narratives, external power interference, and demographic change? The problem emerged in the 19th-20th century context of decolonization, national self-determination movements, and the territorial reorganization following World War I and World War II. The self-determination reading applies the then-emerging modern principle to this case: legitimacy derives from the actual demographic reality during the modern period, not ancient history or external power decree.
% FOUNDING_PROBLEM_CORROBORATION: International legal bodies, UN mechanisms, and Palestinian leadership attest that the founding problem remains live and that self-determination is the appropriate solution framework. Independent scholarship on decolonization and national sovereignty supports the principle's application. However, proponents of competing readings (covenant-continuity, existential-matrix) and beneficiaries of partition arrangements attest that the problem is either already resolved by other means (existing international recognition, existential necessity) or that self-determination is inapplicable to this case due to contested history. No consensus corroboration exists outside the reading's own constituency; the founding problem's status itself is the reading's core contestation point.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__self_determination_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__self_determination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__self_determination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__self_determination_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__self_determination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__self_determination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   EXTRACTIVENESS (0.82 at 2025): The constraint is substantially extractive because the self-determination principle's application is asserted normatively but actively suppressed materially. The beneficiary (Arab Palestinian population) gains legitimacy recognition but not sovereignty authority or territorial control. The extraction target (Palestinian refugees, occupied residents) bears the cost of a principle that does not restore their status or rights in practice. The measurement series shows extraction rising from minimal (1800, pre-modern-period framing) through rapid escalation (1880–1948, as the principle becomes codified) to plateau (post-1967, stable non-enforcement). The rise reflects increasing articulation of the claim under conditions of increasing non-enforcement—the gap itself is the extraction mechanism. SUPPRESSION (0.79): The constraint requires active suppression because competing historical narratives and legitimacy claims (covenant-continuity, existential-matrix readings) must be foreclosed to maintain the temporal scope (modern period only) and demographic criterion (majority-resident population). This suppression is structural: the territorial occupation, movement restrictions, and denial of Palestinian state apparatus represent institutional enforcement of the suppression. THEATER (0.41): A substantial share of enforcement activity is performative—UN resolutions reaffirming self-determination, human rights documentation of violations, diplomatic rhetoric invoking the principle—while material enforcement (actual sovereignty transfer) remains absent. Theater has risen over the interval as rhetorical affirmation has increased while material change has not. ACCESSIBILITY_COLLAPSE (0.68): Once the self-determination principle is understood as applicable, alternatives collapse substantially. Other temporal framings (ancient history, pre-modern occupation) are foreclosed by the modern-period scope. Existential-matrix logic (zero-sum survival imperative) is closed off by the juridical-demographic criterion. Covenant-continuity claims are set aside by emphasis on modern period residence. However, collapse is not complete (0.68, not 0.90+) because competing readings remain live in international discourse and retain institutional support. RESISTANCE (0.84): High resistance from both the reading's beneficiaries (Palestinians seeking enforcement) and from those who reject the reading entirely (Israeli government, covenant-continuity advocates, existential-matrix proponents). The constraint meets powerful resistance because it challenges existing territorial arrangements and requires redistribution of sovereignty authority.
 *
 * PERSPECTIVAL GAP:
 *   AGENDA-SETTER vs. PAYER SEATS DIVERGE SHARPLY: From the Arab nationalist movements and Palestinian Authority perspective (agenda-setter seat), this reading is a genuine coordination solution to a legitimacy problem—it provides a universal, transparent standard (modern self-determination) for determining statehood rights, replacing arbitrary power and contested history. From this seat, enforcement is the outstanding question; the principle is sound and deserves implementation. From the Palestinian refugee/diaspora perspective (payer seat), the reading is a source of extraction because it legitimates a claim that does NOT restore their material status or rights. The reading frames their displacement as unjust but leaves them in limbo—neither recognized as having valid territorial rights nor compensated for losses. From their seat, the principle is utopian and operationally extractive. From the external-powers seat (institutional agenda-setter that pronounces but does not enforce), the reading is valuable precisely for its non-enforcement: it allows rhetorical commitment to the universal principle while maintaining strategic interests in the territorial status quo. The engine computes these seat divergences from the structural data—beneficiary/victim declarations, power atoms, exit options—not from the author's claim. IDENTITY-LOCK DYNAMICS: Palestinian political identity is substantially fused with the self-determination principle (it is the primary modern legitimacy claim available to them). An identity_locked exit option for Palestinian populations reflects this fusion—exit from the self-determination framing would require abandoning the primary grounds for national recognition and state claims, which is existentially difficult even where it might be strategically advantageous. This lock amplifies extraction: Palestinians who recognize the principle as non-enforceable cannot easily exit to alternative legitimacy frameworks without losing their standing in international discourse.
 *
 * DIRECTIONALITY LOGIC:
 *   Arab Palestinian population (beneficiary, moderate-to-organized power, constrained exit): d ≈ 0.15–0.25. Benefits from legitimacy recognition (extracted from the principle itself), but exit is constrained by identity-lock (the principle is the primary ground of their national claim) and by power asymmetry (they cannot single-handedly enforce it). Their directionality is beneficiary-side but tempered by the non-enforcement of the benefit. Palestinian refugees/diaspora (victim, powerless, trapped exit): d ≈ 0.85–0.95. Bear the full cost of a principle that frames their displacement as illegitimate but does not restore their status. No meaningful exit exists; they are trapped in both the geographic and political sense. Palestinian occupied-territory residents (secondary_role dual, moderate power, constrained exit): d ≈ 0.55–0.65. Benefit from legitimacy claim but pay through occupation and non-enforcement. Exit is constrained (movement restrictions, lack of state apparatus). External powers (agenda-setter, institutional, mobile exit): d ≈ 0.10–0.20. Nominally supportive of the principle but materially enforce its non-application. Their extraction comes from the gap between affirmation and suppression—they gain strategic flexibility by pronouncing the principle while preserving the territorial status quo. Their mobility means they could shift to enforcement or alternative frameworks, so their d is low (beneficiary-side of the agenda-setter role).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY ASSESSMENT: The founding problem (how to determine legitimacy in a contested territory with competing historical narratives) remains LIVE and UNRESOLVED. The self-determination principle was indeed built to address this problem. However, the constraint now exhibits mandatrophy symptoms: the principle is operationalized rhetorically and diplomatically (high theater ratio, 0.41) but not enforced materially (suppression remains active, occupation persists, displacement is unresolved). The gap between affirmation and enforcement is widening, not narrowing (extractiveness rising from 1920–1967 period, then plateauing at high level 1967–2025). This pattern—living founding problem, rising theater, stable high extraction—is characteristic of mandatrophy: the constraint is maintained not because it solves the problem but because it provides rhetorical cover for a status quo that benefits external powers and established state actors. The reading would be reclassified from tangled_rope (genuine coordination function + asymmetric extraction) to snare (no meaningful coordination function, pure extraction maintained by suppression) if the founding problem were confirmed as dead AND the theater ratio continued rising above 0.60. Currently it sits at mandatrophy boundary: the problem is live, so some coordination claim persists, but enforcement decay combined with rising theater suggests the claim is becoming performative. RESOLUTION MECHANISM: Mandatrophy would be resolved by either (a) enforcement of the self-determination principle (material transfer of sovereignty to Palestinian governance), or (b) explicit abandonment of the principle in favor of alternative legitimacy framings (covenant-continuity, existential-matrix, realpolitik power allocation). The current state—affirmation without enforcement—is the extraction mechanism itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    modern_period_boundary_ambiguity,
    'What precisely constitutes the ''modern period'' (19th-20th centuries) in this reading? Does it begin with Ottoman administrative modernization (~1839), British occupation (~1882), Zionist immigration (~1880s), or some other event? How sensitive is the demographic claim to the boundary chosen?',
    'Explicit historical analysis and demographic reconstruction showing Arab population majority at multiple time points (e.g., 1850, 1900, 1948) under different boundary definitions. Genealogical studies and census data (Ottoman, British Mandate, contemporary) would establish robustness of the claim.',
    'If the boundary shifts earlier, Jewish presence and immigration may qualify as continuous within the modern period, weakening the demographic-majority basis. If later, some Arab displacement becomes exogenous to the modern period''s starting conditions, affecting the justice claim about partition. Boundary choice directly affects the constraint''s empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modern_period_boundary_ambiguity, empirical, 'Temporal scope ambiguity: where does the ''modern period'' begin and how does the choice affect the demographic claim?').

omega_variable(
    continuous_vs_intermittent_presence,
    'Does ''continuous residence'' in this reading require unbroken habitation by the same population, or is periodic displacement and return sufficient? How does the reading account for Bedouin movement patterns, seasonal migration, and Ottoman administrative relocations?',
    'Clarification of the reading''s own standard of continuity through legal or scholarly exegesis; empirical reconstruction of population movement patterns from archival sources, oral history, and genealogical data. Assessment of whether historical precedent in self-determination doctrine requires absolute continuity or allows return-from-displacement.',
    'A strict continuity requirement may exclude populations with disrupted residence; a looser standard may admit populations with historical ties but interrupted habitation. The reading''s legitimacy claim relies on the specificity of the continuity criterion; ambiguity here opens space for competing interpretations (covenant-continuity reading may claim Jewish continuous presence via diaspora ties and periodic pilgrimage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuous_vs_intermittent_presence, conceptual, 'What standard of ''continuous residence'' grounds the demographic legitimacy claim and how is it applied?').

omega_variable(
    universality_vs_selectivity_gap,
    'The self-determination principle is stated as a universal doctrine (all peoples have the right to self-determination), yet this reading''s enforcement is highly particular and contested. Does selective enforcement undermine the principle''s legitimacy, or is selectivity an inevitable feature of any universal principle applied in a world of power asymmetries?',
    'Comparative analysis: examining how self-determination principle is applied or denied in other territorial disputes (Kashmir, Taiwan, Northern Ireland, Crimea, etc.). Assessment of whether consistent application across cases strengthens or weakens the constraint''s authority; study of whether selective application converts universal principle into cover story for geopolitical preference.',
    'If selectivity is revealed as systematic (principle invoked for some peoples, denied for others, based on geopolitical interests), the constraint''s legitimacy shifts from principled to extractive cover. If selectivity is unsystematic or contested in good faith, the principle retains some binding force. This bears on classification: consistent non-enforcement + rising theater + power-asymmetric beneficiary/victim structure = mandatrophy boundary or snare. Systematic selectivity would support snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universality_vs_selectivity_gap, empirical, 'Is the principle''s selective enforcement a feature of legitimate contextual judgment or evidence of masked extraction?').

omega_variable(
    covenant_continuity_reading_foreclosure,
    'Does this reading''s temporal scope (modern period only) logically foreclose the covenant-continuity reading, or do they operate in different legitimacy frameworks that can coexist? Can an adjudicator accept both modern self-determination AND ancient covenant as legitimate grounds simultaneously?',
    'Jurisprudential analysis: examining whether international law and precedent treat temporal scope as mutually exclusive (only the most recent legitimate claim counts) or cumulative (multiple temporal groundings may reinforce). Study of how courts and international bodies handle competing historical claims to the same territory.',
    'If temporal scopes foreclose each other (one reading rules out the other), the classification is forecloses; if they coexist as competing claims that different parties hold simultaneously, the relation is coexists_with. The resolution affects the cs_structure.reading_relations field and how the engine models the kernel''s internal structure. Currently authored as coexists_with, reflecting the empirical fact that both readings are held by different parties; foreclosure would require that accepting modern self-determination logically excludes ancient covenant claims, which is not universally agreed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(covenant_continuity_reading_foreclosure, conceptual, 'Is the self-determination reading logically incompatible with the covenant-continuity reading, or can both be held by different parties?').

omega_variable(
    existential_matrix_relation_to_self_determination,
    'Can the existential-matrix reading (zero-sum survival imperative) be reconciled with the self-determination reading (universal juridical principle), or are they fundamentally opposed? If both peoples frame territorial control as existential necessity, does self-determination principle become inapplicable or require a choice between existential claims?',
    'Philosophical analysis of whether existential imperatives can ground self-determination claims universally (each people''s survival need justifies sovereignty), or whether existential and juridical framings are incommensurable. Empirical study of how the two readings are deployed rhetorically: do proponents of existential logic argue it supersedes self-determination, or that it provides a deeper grounding for self-determination?',
    'If existential and juridical logics are incommensurable, the relation is forecloses. If existential logic provides a deeper ground for self-determination (survival need justifies a people''s right to govern themselves), the relation is influences: existential-matrix reading creates downstream pressure that reframes self-determination as derivable from survival imperative. Currently authored as coexists_with, reflecting empirical coexistence; resolution could shift the relation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(existential_matrix_relation_to_self_determination, conceptual, 'Are the existential-matrix and self-determination readings logically compatible or competing frameworks for legitimacy?').

omega_variable(
    international_enforcement_capacity_decay,
    'Is the rising theater ratio (0.41 at 2025 vs. 0.05 at 1800) evidence of degradation in the self-determination principle''s enforcement mechanisms, or a feature of increasing rhetorical commitment despite material constraints? Is the constraint approaching piton status (performative maintenance of an atrophied function)?',
    'Analysis of international enforcement mechanisms (UN enforcement authority, jurisdiction of international courts, capacity of member-states to enforce resolutions). Comparison of enforcement patterns in other self-determination cases (Kosovo, East Timor, South Sudan) to assess whether the constraint''s enforcement capacity has actually decayed or whether theater is inherent to the principle''s universal application.',
    'If enforcement capacity is decaying (institutional mechanism weakening), the constraint is approaching piton status: the founding problem remains live but the mechanism for solving it is atrophying, yet the constraint persists through theatrical commitment. If theater is systemic to universal principles applied in power-asymmetric contexts, the high theater ratio is a feature of the principle, not a signal of decay. Classification consequence: piton diagnosis would require explicit acknowledgment in base_properties flags.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_enforcement_capacity_decay, empirical, 'Is rising theater ratio evidence of enforcement decay and incipient piton status?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__self_determination_reading, 1800, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1800, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement_basis(terr_tr_t1800, projected).
narrative_ontology:measurement(terr_tr_t1880, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1880, 0.12).
narrative_ontology:measurement_basis(terr_tr_t1880, projected).
narrative_ontology:measurement(terr_tr_t1920, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1920, 0.25).
narrative_ontology:measurement_basis(terr_tr_t1920, observed).
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1948, 0.32).
narrative_ontology:measurement_basis(terr_tr_t1948, observed).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 1967, 0.37).
narrative_ontology:measurement_basis(terr_tr_t1967, observed).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement_basis(terr_tr_t2000, observed).
narrative_ontology:measurement(terr_tr_t2025, territorial_sovereignty_legitimacy__self_determination_reading, theater_ratio, 2025, 0.41).
narrative_ontology:measurement_basis(terr_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(terr_be_t1800, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1800, 0.15).
narrative_ontology:measurement_basis(terr_be_t1800, projected).
narrative_ontology:measurement(terr_be_t1880, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1880, 0.32).
narrative_ontology:measurement_basis(terr_be_t1880, projected).
narrative_ontology:measurement(terr_be_t1920, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1920, 0.54).
narrative_ontology:measurement_basis(terr_be_t1920, observed).
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1948, 0.71).
narrative_ontology:measurement_basis(terr_be_t1948, observed).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 1967, 0.76).
narrative_ontology:measurement_basis(terr_be_t1967, observed).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2000, 0.8).
narrative_ontology:measurement_basis(terr_be_t2000, observed).
narrative_ontology:measurement(terr_be_t2025, territorial_sovereignty_legitimacy__self_determination_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(terr_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1800, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement_basis(terr_su_t1800, projected).
narrative_ontology:measurement(terr_su_t1880, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1880, 0.38).
narrative_ontology:measurement_basis(terr_su_t1880, projected).
narrative_ontology:measurement(terr_su_t1920, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1920, 0.58).
narrative_ontology:measurement_basis(terr_su_t1920, observed).
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1948, 0.72).
narrative_ontology:measurement_basis(terr_su_t1948, observed).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement_basis(terr_su_t1967, observed).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2000, 0.78).
narrative_ontology:measurement_basis(terr_su_t2000, observed).
narrative_ontology:measurement(terr_su_t2025, territorial_sovereignty_legitimacy__self_determination_reading, suppression_requirement, 2025, 0.79).
narrative_ontology:measurement_basis(terr_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__self_determination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__self_determination_reading, 0.14).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__self_determination_reading, territorial_sovereignty_legitimacy__existential_matrix_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'territorial_sovereignty_legitimacy'. The three readings (self-determination, covenant-continuity, existential-matrix) share the same referent (the standing commitment to determine what makes a state's territorial claim legitimate) but instantiate different readings with different temporal scopes, legitimacy criteria, and structural beneficiary/victim maps. Each reading has its own ε value (extractiveness is reading-indexed, per OQ-26), its own stakeholder surface, and its own classification. They are linked by network.affects_constraints to enable contamination and coupling analysis across the kernel. Self-determination reading influences both siblings through its universal principle claim and its modern-period temporal scope, which constrain how the other readings must position themselves. See commentary.kernel_context and cs_structure sections for the sibling relationships and axiom distinctions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
