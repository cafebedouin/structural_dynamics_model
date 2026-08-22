% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__settler_colonial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__settler_colonial_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__settler_colonial_reading
 *   human_readable: Zionism as Settler-Colonial Displacement (Structural Reading)
 *   domain: political_philosophy/postcolonial_theory/nationalism_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the settler-colonial reading of
 *   Zionism and Jewish immigration to Palestine. It does NOT adjudicate
 *   whether Zionism is justified, whether Jewish refuge-seeking was
 *   legitimate, or whether contemporary Israeli statehood is valid. It
 *   specifies a structural reading: the constraint is the apparatus of
 *   territorial acquisition, property law, military enforcement, and
 *   political representation that systematically transfers control of
 *   Palestinian land to Jewish-majority institutions, accompanied by the
 *   displacement and legal subordination of Palestinians. The reading
 *   examines this transfer AS A STRUCTURAL MECHANISM whose operation
 *   resembles documented settler-colonial patterns (Algeria, South Africa,
 *   North America, Australia) regardless of the subjective humanitarian or
 *   defensive motivations of individual participants. The claim/metric
 *   independence is critical here: the constraint is CLAIMED as a snare (pure
 *   extraction through coercion, cover story of security and
 *   self-determination) while the authored metrics describe the actual
 *   measurement. The engine will compute per-seat types; this reading's
 *   seat-divergence is the entire point.
 *
 * KEY AGENTS:
 *   - Palestinians: dispossessed indigenous population; powerless exit; primary victims
 *   - Jewish immigrants and refugees: positioned as settlers structurally; identity-locked exit; trauma-bearing agents
 *   - British imperial authority: agenda-setter during mandate period; extracted strategic advantage
 *   - U.S. imperial interests: later beneficiary and military enforcer; extracted regional dominance
 *   - Arab states: excluded from legitimacy deliberation; constrained intervention capacity
 *   - International legal observers: witness the pattern; lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, 0.85).
domain_priors:suppression_score(jewish_sovereignty_palestine__settler_colonial_reading, 0.78).
domain_priors:theater_ratio(jewish_sovereignty_palestine__settler_colonial_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__settler_colonial_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__settler_colonial_reading, snare).
narrative_ontology:human_readable(jewish_sovereignty_palestine__settler_colonial_reading, "Zionism as Settler-Colonial Displacement (Structural Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__settler_colonial_reading, "political_philosophy/postcolonial_theory/nationalism_studies").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__settler_colonial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__settler_colonial_reading, '1f12bca9-8731-4a25-8cdf-1aea629157b4').
narrative_ontology:cs_kernel_codification('1f12bca9-8731-4a25-8cdf-1aea629157b4', formalized).
narrative_ontology:cs_authority_grounding('1f12bca9-8731-4a25-8cdf-1aea629157b4', extraction).
narrative_ontology:cs_interpretation_layer_present('1f12bca9-8731-4a25-8cdf-1aea629157b4').
narrative_ontology:cs_reading_relation('1f12bca9-8731-4a25-8cdf-1aea629157b4', jewish_sovereignty_palestine__liberal_nationalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f12bca9-8731-4a25-8cdf-1aea629157b4', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f12bca9-8731-4a25-8cdf-1aea629157b4', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_reading_relation('1f12bca9-8731-4a25-8cdf-1aea629157b4', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('1f12bca9-8731-4a25-8cdf-1aea629157b4', foundational, indigenous_territorial_primacy).
narrative_ontology:cs_axiom_status(indigenous_territorial_primacy, holdable).
narrative_ontology:cs_axiom_grounding('1f12bca9-8731-4a25-8cdf-1aea629157b4', indigenous_territorial_primacy, deontological).
narrative_ontology:cs_axiom('1f12bca9-8731-4a25-8cdf-1aea629157b4', foundational, settler_colonialism_structural_pattern).
narrative_ontology:cs_axiom_status(settler_colonialism_structural_pattern, holdable).
narrative_ontology:cs_axiom_grounding('1f12bca9-8731-4a25-8cdf-1aea629157b4', settler_colonialism_structural_pattern, empirically_contingent).
narrative_ontology:cs_reference_frame('1f12bca9-8731-4a25-8cdf-1aea629157b4', pre_zionist_palestinian_autonomy).
narrative_ontology:cs_drift_state('1f12bca9-8731-4a25-8cdf-1aea629157b4', contemporary_2025, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('1f12bca9-8731-4a25-8cdf-1aea629157b4', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, later_us_imperial_interests).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, palestinians).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, displaced_arab_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__settler_colonial_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Indigenous population displaced from ancestral territory through systematic land acquisition, legal exclusion, and military enforcement. Face loss of property, political representation, and territorial control regardless of the subjective intentions of individual Jewish immigrants. Lack effective exit options: return to lands is legally and militarily prevented; integration offered only on terms that require abandoning collective identity claims; international advocacy produces no structural remedy.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, palestinians, payer,
    powerless, generational, trapped, regional).

% Fleeing European persecution and seeking territorial refuge, they are positioned structurally as settlers regardless of subjective refugee status or humanitarian motivation. Their identity and survival needs are bound to the territorial project; exit from participation in the constraint (land acquisition, settlement expansion) is experienced as identity dissolution. They carry out the dispossession machinery while bearing their own trauma.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_refugees, payer,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_immigrants_and_refugees, excluded).

% British mandate authority (1920–1948) facilitated Jewish immigration, land purchases, and institutional development under the framework of the Balfour Declaration. Extracted strategic advantage: a client settler state in the eastern Mediterranean, division of Arab nationalism, control of Palestine as buffer and resource zone. Maintained plausible deniability by framing the project as humanitarian while institutionalizing legal asymmetries favoring Jewish acquisition and Palestinian exclusion.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests, agenda_setter,
    institutional, generational, arbitrage, global).

% After 1948, the United States became primary beneficiary and enforcer: military aid to maintain Israeli military dominance, diplomatic shield against UN action, economic support. Extracted geopolitical leverage: reliable regional ally, counterweight to Soviet/Arab movements, control of energy-security architecture in the Middle East. The constraint's persistence serves U.S. strategic interests in regional domination.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, later_us_imperial_interests, beneficiary,
    institutional, generational, arbitrage, global).

% Would contest the territorial and political settlement; blocked from effective intervention by military imbalance, great-power backing of the settler state, and institutional isolation. Their objections are not incorporated into the legitimacy framework of the constraint; their interests are treated as obstacles rather than stakeholders.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, arab_states, excluded,
    organized, generational, constrained, regional).

% Witnesses the constraint's operation and records its conduct relative to settler-colonial patterns documented in other contexts (Algeria, South Africa, Australia, North America). Their capacity to enforce remedies is systematically constrained by great-power veto and the strategic value of the constraint to core states.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, international_legal_community, observer,
    institutional, generational, analytical, global).

% Benefit politically and symbolically from the existence of a Jewish majority state that claims to represent Jewish peoplehood globally. Many diaspora institutions are coordinated with Israeli state interests. Some dissenting voices dispute the settler-colonial framing internally but lack institutional power to alter the constraint's operation.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__settler_colonial_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__settler_colonial_reading, jewish_diaspora_communities, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__settler_colonial_reading, british_imperial_interests).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__settler_colonial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The settler-colonial reading does not identify a genuine coordination function that would justify the constraint. The proffered justifications (security, democratic governance, shared values) are framed by this reading as cover stories for territorial acquisition. Some Jewish participants experience survival/refuge as a coordination problem solved by the constraint; the reading argues this solves one problem by inflicting it on another population and thus fails the coordination test. 
% TRANSFER_FUNCTION: Moves Palestinian land, property rights, and political sovereignty to Jewish-majority institutions and the Jewish diaspora (primary beneficiaries in Israeli institutional seats). Moves strategic regional value to British imperial authority (1920–1948) and U.S. imperial interests (1948–2025). Moves security burden and military enforcement cost to Jewish Israeli populations who become locked into maintaining the dispossession apparatus. Moves dispossession, legal subordination, and territorial loss to Palestinians.
% ABSENT_VOICES: Palestinians had no seat in the British mandate authority or Zionist organization deliberations that produced the constraint. Arab states were excluded from legitimacy deliberation. Internal Jewish voices opposing Zionism (ultra-Orthodox anti-Zionists) were overridden by institutional Zionist dominance. Contemporary Palestinian voices advocating for return, equal rights, or self-determination remain structurally excluded from the sovereignty arrangement. Post-Zionist Jewish voices questioning the ethnic-national framework lack institutional power to alter the constraint.
% DISAPPEARANCE_RATIONALE: If the settler-colonial apparatus disappeared—if Israeli military occupation ended, Palestinian territorial control were restored, property claims were adjudicated with Palestinian rights centered, and political representation were restructured around Palestinian self-determination—the entire geopolitical arrangement of the Middle East would reorganize. Israeli statehood as currently constituted would not survive without the territorial control and military dominance the apparatus maintains. The region would rebalance toward Palestinian and Arab state interests. The strategic value U.S. power extracts from Israeli military dominance would evaporate.
% FOUNDING_PROBLEM: European antisemitism created a genuine refugee crisis for Jews in the late 19th and early 20th centuries. Jewish organizations sought territorial refuge and institutional autonomy. Zionist ideology proposed Palestinian territory as the solution, claiming simultaneous emptiness (terra nullius—a factually false claim later disproven by historians) and historical Jewish connection, thus legitimacy for settlement.
% FOUNDING_PROBLEM_CORROBORATION: Historians outside the Zionist institutional framework (Pappé, Said, Masalha, Khalidi, Shlaim) attest that the founding problem (European antisemitism, refugee need) was real but that (1) Palestinian territory was demonstrably not empty, (2) the solution created a new catastrophic problem for Palestinians, and (3) by the 1960s–1980s, diaspora Jewish refuge-seeking was substantially addressed through other channels (immigration to North America, postwar European rehabilitation), yet the territorial apparatus persisted and intensified. The founding mandate has thus expired while the constraint has metastasized. This is mandatrophy: the constraint's original justification is gone; new justifications (security, democracy) are post-hoc.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__settler_colonial_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__settler_colonial_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__settler_colonial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__settler_colonial_reading, 0.85, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__settler_colonial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__settler_colonial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises monotonically from 0.15 (early Zionist immigration, perceived by this reading as the initial displacement mechanism) through 0.85 (contemporary Israeli sovereignty and settlement expansion). The reading characterizes this trajectory as the progressive institutionalization and militarization of dispossession. Theater begins low (1880s: explicit land acquisition and demographic replacement) and rises to moderate levels (0.42 by 2025) as the constraint's operation incorporates security narratives, humanitarian justifications, and democratic-process framing that obscure the underlying zero-sum territorial logic. Suppression requirement rises steeply: from 1880–1920 (early resistance is local and uncoordinated), through 1948 (Palestinian national mobilization increases, suppression intensifies), to 1967–2025 (sustained military occupation and legal asymmetries are required to maintain the constraint). The theater_ratio trajectory reflects the reading's assessment that early explicit land acquisition has been wrapped in layers of justification (security, democracy, shared values) that obscure the structural dispossession.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes that the constraint exhibits extreme seat divergence. From the Israeli security and nationalist seat: Zionism solved a dire historical problem (antisemitism, refugeeism) through legitimate self-determination and territorial consolidation. From the Palestinian seat: it is a structural dispossession apparatus. Neither reading is irrational or dishonest from its own seat; the gap is built into the asymmetry of the constraint. The engine's computation will show Israeli seats computing the type as rope or scaffold (genuine coordination); Palestinian seats will compute it as snare (pure extraction). The prompt to reconcile these is false—the gap is the point.
 *
 * DIRECTIONALITY LOGIC:
 *   Palestinians are trapped victims with near-zero effective exit: return is legally and militarily prevented; integration is offered only on terms requiring identity subordination; armed resistance is militarily crushed. Their d approaches 1.0 (full target). Jewish immigrants carry d in the 0.6–0.8 range: they benefit from the territorial acquisition and eventually from statehood, but they are also identity-locked into the mechanism itself—their exit from the constraint is experienced as identity dissolution and potential loss of refuge. British and U.S. institutional actors carry low d (they are beneficiaries without being the primary extractive machinery: d in the 0.2–0.4 range). The engine's per-seat computation will show dramatic divergence: from the Jewish-Israeli seat, the arrangement solves security and self-determination (low extractiveness, high coordination). From the Palestinian seat, it is pure extraction (high extractiveness, zero coordination). This divergence is the reading's core structural claim.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (European antisemitism) was real and urgent. The reading argues that the solution—settlement in Palestine—solved it FOR Jewish refugees BY DISPLACING Palestinians. The mandate has thus NOT been fulfilled in the sense that would justify the constraint's persistence: it solved one humanitarian crisis by creating another (Palestinian dispossession). By the 1960s, the founding problem (antisemitism, refuge in Europe) was substantially ameliorated for diaspora Jews, yet the territorial apparatus persisted and intensified. The reading identifies mandatrophy: the constraint has outlived any justified function relative to its costs. Contemporary justifications (security, democracy) are new mandates layered on top to defend what was built on the original (now-dead) mandate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intent_vs_structural_effect,
    'Does the structural categorization as settler-colonialism require hostile intent from individual Jewish immigrants, or is the label structural regardless of subjective humanitarian motivations?',
    'Comparative historical analysis: examining whether other settler-colonial cases (Australian, North American, Algerian) required conscious colonialist intent from settlers or whether the structure operated regardless of settlers'' self-understanding. The reading''s position: structure is independent of intent; settlers can sincerely believe they are refugees while operating as settlers structurally.',
    'If structural (independent of intent), the reading stands without requiring either malicious Jewish actors or bad-faith Zionist ideology. If intent-dependent, the argument requires demonstrating conspiracy or deliberate displacement plans, which shifts the burden of proof and enables defensive counters emphasizing humanitarian motivations. This omega resolves whether moral agency is correctly assigned.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intent_vs_structural_effect, conceptual, 'Whether settler-colonial classification requires hostile intent or is purely structural').

omega_variable(
    territorial_entitlement_grounding,
    'On what basis does the settler-colonial reading deny or override Jewish territorial claims that the liberal-nationalist and religious-zionist readings ground in self-determination rights or divine promise? Is there a principled secular argument, or does this reading require a prior commitment to indigenous-rights primacy?',
    'Philosophical analysis of rights hierarchy: do self-determination rights of an ethno-national diaspora trump land-use rights of an indigenous population? Can both be satisfied non-territorially? The settler-colonial reading presupposes indigenous-rights primacy without argumentation; the omega documents this presupposition.',
    'Resolving this omega determines whether the reading is self-contained or parasitic on a prior value commitment (indigenous rights over diaspora rights). If parasitic, the reading remains valid but is not neutral ground—it is one value system among others. If independent secular grounding exists, the reading''s force is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_entitlement_grounding, preference, 'Whether indigenous-rights primacy is derivable or foundational to this reading').

omega_variable(
    imperial_beneficiary_continuity,
    'Is the U.S. imperial interest in contemporary Israeli statehood causally necessary to the constraint''s persistence, or is Israeli institutional and military power now independent of great-power backing?',
    'Counterfactual analysis: what would happen to Israeli state capacity, territorial control, and settlement expansion if U.S. military aid, diplomatic cover, and technology transfer ceased? Economic modeling of Israeli resilience. Comparative cases of states losing great-power backing.',
    'If U.S. backing is causally necessary, the constraint is still a snare serving imperial interests (the beneficiary category holds). If Israeli power is now independent, the constraint has transformed into a self-sustaining regional domination system (still extractive, but the beneficiary is no longer primarily imperial—it is Israeli institutional interests). This shifts the diagnosis from imperialism to settler-state entrenchment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(imperial_beneficiary_continuity, empirical, 'Whether Israeli state persistence depends on U.S. imperial backing or is now autonomous').

omega_variable(
    kernel_reading_distinction_from_sibling,
    'Can the settler-colonial reading be coherently distinguished from a political-leftist critique that uses similar language? Where does structural analysis end and ideological judgment begin?',
    'Comparison with the post-zionist reading: both critique Zionism and identify injustice, but post-zionism accepts the achieved statehood as a fact and argues for institutional reform (civic equality, shared governance), while settler-colonialism frames statehood itself as delegitimized by its origin. Does the settler-colonial reading require a different institutional outcome, or is it compatible with post-zionist remedies?',
    'If settler-colonialism is compatible with post-zionist remedies, the readings are primarily different in historical diagnosis, not institutional vision. If settler-colonialism requires reversal of statehood or return, the readings diverge on what justice requires. This omega documents the boundary between structural analysis and prescriptive ideology.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_distinction_from_sibling, conceptual, 'Whether settler-colonial reading is structurally distinct from post-zionist reading or primarily differs in prescriptive framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__settler_colonial_reading, 1880, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1880, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1880, 0.08).
narrative_ontology:measurement(jewi_tr_t1920, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1920, 0.18).
narrative_ontology:measurement(jewi_tr_t1948, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(jewi_tr_t1967, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1967, 0.35).
narrative_ontology:measurement(jewi_tr_t1987, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 1987, 0.39).
narrative_ontology:measurement(jewi_tr_t2025, jewish_sovereignty_palestine__settler_colonial_reading, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1880, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1880, 0.15).
narrative_ontology:measurement(jewi_be_t1920, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1920, 0.42).
narrative_ontology:measurement(jewi_be_t1948, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1948, 0.68).
narrative_ontology:measurement(jewi_be_t1967, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1967, 0.76).
narrative_ontology:measurement(jewi_be_t1987, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 1987, 0.81).
narrative_ontology:measurement(jewi_be_t2025, jewish_sovereignty_palestine__settler_colonial_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t1880, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1880, 0.22).
narrative_ontology:measurement(jewi_su_t1920, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1920, 0.48).
narrative_ontology:measurement(jewi_su_t1948, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1948, 0.61).
narrative_ontology:measurement(jewi_su_t1967, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(jewi_su_t1987, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 1987, 0.75).
narrative_ontology:measurement(jewi_su_t2025, jewish_sovereignty_palestine__settler_colonial_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__settler_colonial_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__settler_colonial_reading, 0.15).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__cultural_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__settler_colonial_reading, jewish_sovereignty_palestine__post_zionist_reading).

% DUAL FORMULATION NOTE:
% The jewish_sovereignty_palestine kernel instantiates five distinct constraint readings, each with different beneficiary/victim structures, extractiveness assessments, and type classifications. This story (settler_colonial_reading) specifies the referent as the zero-sum territorial apparatus and measures extraction relative to Palestinian dispossession. The liberal_nationalist_reading measures extraction relative to Jewish self-determination gains. The post_zionist_reading accepts statehood as achieved but diagnoses mandatrophy in the ethnic-national framework. All five readings examine the same historical events and institutional machinery; they differ in which structural elements are foregrounded and what counts as coordination vs. extraction. No single reading can accommodate the others; they represent genuinely opposed interpretations of a contested kernel. The engine will compute per-reading per-seat classifications; the divergence across readings is the measurement the corpus is designed to capture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, powerless, 0.92).
constraint_indexing:directionality_override(jewish_sovereignty_palestine__settler_colonial_reading, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
