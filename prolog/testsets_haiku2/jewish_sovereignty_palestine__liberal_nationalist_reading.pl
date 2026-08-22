% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination and Statehood in Palestine (Liberal Nationalist Reading)
 *   domain: political_philosophy/nationalism_studies/postcolonial_theory
 *
 * SUMMARY:
 *   The liberal nationalist reading of the Jewish sovereignty dispute frames
 *   the constraint as an exercise of collective self-determination rights for
 *   both Jewish and Palestinian peoples. Under this reading, Jewish statehood
 *   in the ancestral homeland is legitimate as the instantiation of Jewish
 *   national self-governance; Palestinian statehood or coexistence within a
 *   binational framework is equally legitimate as Palestinian
 *   self-determination. The constraint is tangled rope: it coordinates two
 *   national projects and solves a genuine problem (how to institutionalize
 *   self-determination for peoples with overlapping territorial claims) while
 *   imposing asymmetric costs—primarily on Palestinians through territorial
 *   partition and restrictions on return, justified within the reading as
 *   necessary to maintain Jewish demographic majority and thus the state's
 *   viability as a Jewish state. The reading explicitly rejects both the
 *   religious-zionist claim that the territory belongs exclusively to Jews by
 *   divine covenant and the settler-colonial reading that portrays all Jewish
 *   settlement as inherently displacement. It occupies the middle ground:
 *   mutual recognition of self-determination rights, but crystallized into a
 *   territorial partition that privileges Jewish institutional continuity.
 *
 * KEY AGENTS:
 *   - Jewish collective as nation — possesses self-determination right; benefits from institutional statehood; agenda-setter for state apparatus
 *   - Palestinian collective as nation — possesses equal self-determination right; bears territorial and political costs of partition; constrained to coexistence framework
 *   - Liberal nationalist theorists — provide epistemic grounding for the reading's coherence; occupy observer seat
 *   - International legal community — partially excluded; their authority is subordinated to the self-determination principle as the reading frames it
 *   - Settler-colonial and post-zionist critics — excluded from the reading's internal legitimacy structure; their alternative framings are pre-negated
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.58).
domain_priors:suppression_score(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.42).
domain_priors:theater_ratio(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__liberal_nationalist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__liberal_nationalist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__liberal_nationalist_reading, "Jewish Self-Determination and Statehood in Palestine (Liberal Nationalist Reading)").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__liberal_nationalist_reading, "political_philosophy/nationalism_studies/postcolonial_theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__liberal_nationalist_reading, 'fa1b348a-b916-413b-b24c-4a7324353dc8').
narrative_ontology:cs_kernel_codification('fa1b348a-b916-413b-b24c-4a7324353dc8', fixed_text).
narrative_ontology:cs_authority_grounding('fa1b348a-b916-413b-b24c-4a7324353dc8', lineage).
narrative_ontology:cs_interpretation_layer_present('fa1b348a-b916-413b-b24c-4a7324353dc8').
narrative_ontology:cs_reading_relation('fa1b348a-b916-413b-b24c-4a7324353dc8', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa1b348a-b916-413b-b24c-4a7324353dc8', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa1b348a-b916-413b-b24c-4a7324353dc8', jewish_sovereignty_palestine__cultural_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fa1b348a-b916-413b-b24c-4a7324353dc8', jewish_sovereignty_palestine__post_zionist_reading, influences).
narrative_ontology:cs_axiom('fa1b348a-b916-413b-b24c-4a7324353dc8', foundational, jewish_palestinian_self_determination_coequal).
narrative_ontology:cs_axiom_status(jewish_palestinian_self_determination_coequal, holdable).
narrative_ontology:cs_axiom_grounding('fa1b348a-b916-413b-b24c-4a7324353dc8', jewish_palestinian_self_determination_coequal, deontological).
narrative_ontology:cs_axiom('fa1b348a-b916-413b-b24c-4a7324353dc8', foundational, territorial_partition_or_binational_coexistence_necessary).
narrative_ontology:cs_axiom_status(territorial_partition_or_binational_coexistence_necessary, holdable).
narrative_ontology:cs_axiom_grounding('fa1b348a-b916-413b-b24c-4a7324353dc8', territorial_partition_or_binational_coexistence_necessary, instrumental).
narrative_ontology:cs_reference_frame('fa1b348a-b916-413b-b24c-4a7324353dc8', liberal_nationalist_mutual_self_determination).
narrative_ontology:cs_drift_state('fa1b348a-b916-413b-b24c-4a7324353dc8', contemporary_state_security_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fa1b348a-b916-413b-b24c-4a7324353dc8', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_diaspora_and_refugees).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under this reading, the Jewish people possess a collective right to self-determination and the exercise of that right through statehood in territory they claim as ancestral homeland. The beneficiary is the collective itself—the nation—not individual Jews. The constraint legitimates the establishment and defense of a Jewish state as the institutionalization of that right. The reading frames Jewish statehood as a rights-exercise, not occupation: the state apparatus represents the nation's self-governance. Exit would mean renouncing the self-determination claim itself or the state mechanism that instantiates it—structurally identity-locked for those who hold this reading as foundational.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, beneficiary,
    institutional, civilizational, identity_locked, universal).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation, agenda_setter).

% Under this liberal nationalist reading, Palestinians are recognized as possessing an equal self-determination right. The constraint imposes territorial, political, and resource costs on the Palestinian collective: sovereignty is divided, territorial control is circumscribed, and the arrangement requires Palestinian recognition of Jewish statehood and the legitimacy of Jewish self-determination. The reading explicitly frames this as requiring partition or a binational framework—Palestinian self-determination must coexist with Jewish statehood. Exit options are constrained: they cannot easily renounce their own self-determination claim without renouncing nationhood itself, yet accepting the constraint means accepting territorial compromise and coexistence with a state built on Jewish national identity.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_collective_as_nation, payer,
    organized, civilizational, constrained, universal).

% Jewish diaspora communities outside the state benefit indirectly from the legitimation of Jewish nationhood and statehood: it provides institutional representation of Jewish collective identity and a territorial anchor for diaspora belonging. However, they do not directly bear the costs of territorial partition or the enforcement machinery maintaining the state. Their mobility and non-identity-locked status differentiates them structurally from those whose identity is constituted by the state itself.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_diaspora_communities, beneficiary,
    moderate, biographical, mobile, global).

% Palestinian diaspora communities and refugees bear costs imposed by the territorial partition and the establishment of a Jewish state: they face restrictions on return (framed by the reading as necessary to maintain Jewish demographic majority and thus the state's character as Jewish), legal exclusion, and statelessness for many. Their exit options are near-zero: they cannot renounce Palestinian nationhood or territorial claims without erasing themselves; they are trapped by the constraint's distribution of territory and belonging.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, palestinian_diaspora_and_refugees, payer,
    powerless, civilizational, trapped, global).

% Theorists and scholars who hold or analyze the liberal nationalist reading observe the constraint's operation: they provide epistemic grounding for the reading by articulating the coherence of both Jewish and Palestinian self-determination claims within liberal nationalism and by producing arguments for partition/binational coexistence. They occupy an analytical rather than directly organizing seat but their work legitimates the constraint's framing.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, liberal_nationalist_theorists, observer,
    analytical, civilizational, analytical, universal).

% International law bodies and human-rights institutions are partially excluded from the constraint's internal adjudication: the liberal nationalist reading makes international law a secondary authority compared to the self-determination principle itself. Where international law contradicts the reading's framing (e.g., on refugee return, settlement legality, or territorial acquisition), those voices are systematically marginalized. They would argue for different primacy orderings but are institutionally outside the reading's legitimacy framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, international_legal_community, excluded,
    institutional, civilizational, constrained, universal).

% Those who hold sibling readings (settler-colonial, post-zionist) are excluded from the liberal nationalist reading's internal deliberation structure: the reading defines its core premise (Jewish self-determination + Palestinian self-determination) in a way that pre-excludes interpretations that deny either claim or that read the territorial arrangement as inherently exploitative regardless of mutual recognition. These critics would argue the reading mischaracterizes the power asymmetries and historical trajectory but their frameworks are structurally not-at-the-table in the constraint as authored.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__liberal_nationalist_reading, settler_colonial_and_post_zionist_critics, excluded,
    organized, civilizational, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_collective_as_nation).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Institutionalizes the recognition of two co-equal self-determination rights—Jewish and Palestinian—and coordinates their exercise through territorial partition or binational arrangements. Solves the problem: how can two peoples each claim self-determination over overlapping territory? The reading's answer: through formal legal recognition of both claims and institutional coexistence.
% TRANSFER_FUNCTION: Transfers territory, political sovereignty, and control of security/resources from a shared domain into two distinct nation-states or a shared binational framework. Moves recognition of Jewish national legitimacy to the Palestinian collective (and vice versa), requiring each to acknowledge the other's self-determination right. Moves diaspora Jewish identity into state-institutional form and diaspora Palestinians into defined territorial and political status.
% ABSENT_VOICES: Settler-colonial critics argue the reading mischaracterizes historical displacement and power asymmetry—they are excluded by the reading's foundational premise that both claims are equally legitimate. Post-zionist critics contend that ethnic-national statehood obstructs civic equality—excluded by the reading's framing of national self-determination as the primary good. Religious-national voices argue the reading's secular nationalism abandons the theological covenant—excluded by its liberal-secular framework. Communities within both Jewish and Palestinian collectives who reject the national framing entirely (non-nationalist Jews, Palestinian communists, etc.) are also excluded.
% DISAPPEARANCE_RATIONALE: If the liberal nationalist reading's legitimating framework disappeared—if the idea that Jewish and Palestinian peoples each possess collective self-determination rights ceased to be a primary organizing principle—the territorial arrangement, the state structures, and the international legal frameworks built on mutual recognition of those rights would become incoherent. Statehood and partition would lose their primary justification; alternative arrangements (integration, shared governance, hierarchical domination) would become structurally possible. The disappearance of this reading would not automatically produce any particular alternative arrangement, but it would remove the legitimation framework that makes the current partition coherent.
% FOUNDING_PROBLEM: The Jewish people, having experienced statelessness, persecution, and displacement across centuries, lack institutional capacity for self-governance and collective security. The founding problem for this reading is specifically: how can a people denied statehood and sovereignty exercise their self-determination right? The answer: through the establishment of a nation-state in territory with historical and cultural ties to Jewish identity.
% FOUNDING_PROBLEM_CORROBORATION: The reading's own theorists (Kymlicka, Yael Tamir, and liberal nationalist scholars) attest the founding problem was live at the time the reading crystallized (mid-20th century) and remains relevant for diaspora Jewish communities lacking political sovereignty. Palestinian theorists and post-zionist critics attest the founding problem is either dead (Jews achieved security through other means) or was never the real problem (the real driver was European Zionist ideology and colonial circumstance, not Jewish self-determination per se). Independent historians and legal scholars outside both beneficiary groups document the historical contestation: some argue the founding problem was genuine and urgent; others argue it was used to legitimize displacement. The fact of that contestation—corroborated by sources outside both beneficiary parties—is itself the most honest corroboration available.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at 0.58 rather than higher because the reading explicitly recognizes Palestinian self-determination as co-equal and frames territorial partition as compromise rather than conquest. If the reading fully denied Palestinian rights, extractiveness would be near 0.8+; if it fully denied Jewish rights, it would revert to a post-zionist frame. Measured at 0.58, it reflects the reading's own structural claim: both peoples have valid claims on overlapping territory, and the settlement extracts costs (primarily territorial, on Palestinians) but is justified as the price of institutional coexistence. Suppression is moderate (0.42) because the reading frames the constraint as mutual recognition, not coercion—yet suppression does rise modestly over time (0.35 → 0.42) as enforcement machinery hardens to maintain the state's character and boundaries. The theater ratio climbs slowly from 0.15 to 0.28: at t=0 the reading's legitimation (mutual recognition, self-determination as primary good) carries real coordinating function; over time, more enforcement activity defends territorial boundaries and state prerogatives rather than instantiating mutual recognition. The trajectory reflects the reading's own internal tension: the founding justification (self-determination for both) becomes decreasingly adequate to explain the state's behavior (land expansion, refugee exclusion, settlement growth), forcing more of the machinery into performative maintenance of the reading's coherence. Accessibility collapse is low (0.45) because the reading leaves alternatives technically available: binational arrangements, integration, international trusteeship—these are not physically impossible, only politically delegitimated by the reading itself. Resistance is high (0.72) because the constraint meets sustained challenge from multiple directions: those who deny the reading's premise that both claims are co-equal (settler-colonial critics who say Jewish claims erase Palestinian presence; post-zionists who say ethnic nationalism obstructs equality).
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (Jewish state apparatus) and the beneficiary-as-nation experience this constraint as rightful self-governance—they are solving a real problem (collective security, institutional representation). The payer (Palestinian collective) experiences the constraint as territorial partition imposed by a more powerful group, justified with language of mutual rights that feels hollow when the territorial distribution is asymmetric. Liberal nationalist theorists experience it as intellectually coherent—a framework that can hold both claims simultaneously. Settler-colonial critics experience it as an elaborate cover story for displacement: the 'self-determination' framing obscures the material transfer of land and resources from one people to another. Post-zionists experience it as a reading that solved a 20th-century problem but now obstructs the solution to a 21st-century problem (how to live as civic equals across the same territory). The engine computes each seat's experience from the structural data: power level, exit options (identity-locked vs. mobile), beneficiary/victim status. The authored claim (tangled rope: genuine coordination + asymmetric extraction) sits in the middle of this perspectival range; higher power measurements would push toward snare, lower would push toward rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective benefits structurally from the reading: it receives institutional recognition as a nation, sovereign territory, and international legal standing. For this beneficiary, directionality is low (d near 0.1–0.2)—they are the winners of the arrangement. Palestinians are the payers: they lose territory (or control of territory), face restrictions on return and movement, and must accept that 'self-determination' is constrained to a circumscribed state or a binational framework where another nation's character (Jewish) is codified. For this victim, directionality is high (d near 0.7–0.8). The reading tries to keep both groups as 'coordinated' by framing mutual recognition as the good being solved—but the structural asymmetry is unavoidable: one group gains immediate institutional coherence and territorial control; the other gains formal recognition but loses land and faces existential constraints on its own demographic future. Diaspora Jews sit lower on extraction (d near 0.3) because they do not directly bear enforcement costs; they benefit from identification with a state without the identity-lock of territorial inhabitants. The international legal community has constrained exit (d near 0.6–0.7): they could theoretically reject the reading's framing, but doing so puts them in structural opposition to both major parties and their institutional standing depends on being read as neutral.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Jewish statelessness and vulnerability) was live in the mid-20th century and retains some salience today (diaspora communities still experience precarity, though modern liberal democracies have substantially solved the core problem). The constraint's mandate is increasingly contested: the reading claims the problem is still live and that institutional statehood remains necessary; post-zionist and Palestinian critics argue the founding problem is dead (Jews achieved security through other means; Palestinian dispossession solved nothing for Jewish security) or was never the real driver. The theater ratio climbing from 0.15 to 0.28 suggests the constraint's functional mapping is drifting: more enforcement activity goes to defending territorial boundaries and the state's Jewish character than to solving the actual founding problem (collective security, institutional self-governance). This is the signature of mandatrophy: the founding justification becomes inadequate to explain the ongoing machinery. However, the reading still carries enough coordinating force (mutual recognition of self-determination rights) that classification as pure piton (degraded entirely into performance) would be premature. Tangled rope fits: the coordination is real (both groups do gain some form of institutional standing and mutual legal recognition), but the extraction is real (one group is substantially better positioned than the other), and enforcement is active (the state must continuously defend territorial boundaries and legal frameworks that privilege one nation's character). Mandatrophy is incipient but not resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mutual_recognition_coherence,
    'Can two peoples simultaneously exercise self-determination over overlapping territory through territorial partition without one people''s self-determination being systematically privileged over the other''s?',
    'Long-term institutional stability of any partition arrangement: if one people''s national character and control continue to expand while the other''s are systematically constrained, the mutual recognition framing fails empirically. Alternative resolution: theoretical analysis showing whether the reading''s framework necessarily produces asymmetric outcomes or whether asymmetry arises only from historical contingency.',
    'If mutual recognition proves structurally impossible (one party''s self-determination necessarily expands at the other''s expense), the constraint collapses into snare or settler-colonial reading. If achievable, the reading''s tangled-rope classification holds. If asymmetry is contingent, the constraint remains a high-extractiveness tangled rope dependent on continuous political will to enforce the compromise.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mutual_recognition_coherence, conceptual, 'Whether the liberal nationalist framework can coherently instantiate mutual self-determination or whether it necessarily produces hierarchical outcomes.').

omega_variable(
    founding_problem_persistence,
    'Is Jewish statelessness and vulnerability to persecution still the operative founding problem, or have historical conditions changed such that the problem is dead or was never the real driver?',
    'Genealogical analysis: compare the constraint''s early justifications (mid-20th century) against contemporary framings to identify shifts in the stated problem. Empirical analysis: measure actual security outcomes for diaspora Jews before and after statehood; measure whether statehood increased or decreased the risk of persecution. International law scholarship: assess whether liberal democracies now guarantee protections that statehood is claimed to provide.',
    'If the problem is dead, the constraint enters mandatrophy: the founding justification no longer adequately explains the ongoing enforcement machinery. If the problem was never the real driver (colonial ideology was), the reading mischaracterizes its own genesis and the settler-colonial reading captures it better. If the problem is live, the tangled-rope classification holds and the constraint''s ongoing extraction is justified (to some degree) as the price of solving the real problem.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem remains live or is dead/superseded.').

omega_variable(
    refugee_return_contradiction,
    'Does the liberal nationalist reading''s commitment to Palestinian self-determination coherently allow Palestinians displaced by the state''s establishment to exercise the right of return, or is return logically foreclosed by the reading''s commitment to maintaining Jewish demographic majority and state character?',
    'Close reading of the axioms: if demographic majority is foundational to Jewish self-determination (and the reading asserts it is), and if return would eliminate that majority, then return is foreclosed. If demographic majority is contingent or secondary, return remains open. Binational-framework analysis: do proposals emerging from liberal nationalism actually permit Palestinian return, or do they preserve the demographic constraint under new institutional forms?',
    'If return is foreclosed, Palestinians'' self-determination is conditional and dependent on renouncing a core self-determination claim (right to return). This makes the reading''s ''mutual recognition'' asymmetric and pushes extractiveness higher. If return is coherently available, the reading''s framework is more robust against the charge that it privileges one nation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refugee_return_contradiction, conceptual, 'Whether the reading''s axioms permit Palestinian refugee return or logically exclude it.').

omega_variable(
    identity_lock_asymmetry,
    'Is the Jewish collective''s identity-locked relationship to statehood symmetric with the Palestinian collective''s identity-locked relationship to statehood, or does one people have structurally greater exit options than the other?',
    'Comparative historical analysis: diaspora Jews had (and have) options to remain stateless while maintaining ethnic/religious identity; Palestinians historically did not (now increasingly don''t), as statelessness means legal precarity rather than cultural flourishing. Institutional analysis: for Jews, the state is ONE form of institutional expression of identity; for Palestinians, statehood is THE form being offered in the reading''s framework. If true, the groups'' structural relationships to the constraint differ fundamentally.',
    'Asymmetric identity-lock pushes directionality: Jewish identity is less dependent on statehood (higher exit options even in identity-locked category) than Palestinian identity is dependent on state recognition (trapped by the constraint''s framing). This increases effective extraction on Palestinians and highlights the tangled-rope structure: Jewish coordination is voluntary (identity-locked but mobile diaspora), Palestinian coordination is coerced (trapped by the constraint''s territorial framing).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_lock_asymmetry, empirical, 'Whether the reading imposes identity-lock asymmetrically on the two collective beneficiaries and payers.').

omega_variable(
    settler_colonial_framing_alternative,
    'Does the liberal nationalist reading''s framing of Jewish self-determination adequately account for the material displacement of Palestinians that occurred in establishing the state, or does the reading''s emphasis on rights-exercise obscure a settler-colonial pattern?',
    'Comparative institutional analysis: examine whether the constraint''s operation matches settler-colonial patterns (territorial appropriation, demographic manipulation, legal privileges for one group) regardless of the reading''s normative framing. Historical counterfactual: if Jewish self-determination could have been achieved through other means (e.g., cultural autonomy within existing states, international protections, binational arrangements from the outset), does that suggest the reading''s particular territorial solution was contingent on colonial opportunity rather than necessary?',
    'If the reading does obscure settler-colonial dynamics, the settler-colonial reading captures the constraint better than liberal nationalism does. The tangled-rope classification would remain, but the beneficiary/victim distribution and the reading''s legitimacy would be questioned. If the reading accurately captures the constraint and settler-colonialism is a misreading, the liberal nationalist framing holds and extractiveness remains at the measured 0.58 rather than rising toward 0.75+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_framing_alternative, conceptual, 'Whether liberal nationalism adequately frames or obscures settler-colonial dynamics.').

omega_variable(
    kernel_reading_committer_structure,
    'Is this reading''s core axiom—that Jewish and Palestinian peoples possess co-equal self-determination rights—philosophically defendable as neutral/secular liberal nationalism, or does it necessarily smuggle in assumptions that favor one reading over others?',
    'Philosophical analysis: examine whether ''self-determination'' as the liberal nationalist reading defines it (territorial statehood, demographic majority as necessary condition, exit from the arrangement impossible) is generic or reading-specific. Compare against religious zionist and post-zionist definitions of what self-determination requires. If the reading''s definition is non-generic, it is not merely instantiating a neutral principle but enforcing a particular reading of what the principle means.',
    'If the reading''s framework necessarily privileges itself over alternatives, it operates more like a commitment-system constraint than a neutral principle-application. The reading would be better classified as an exercise of institutional power (the liberal-nationalist establishment enforcing its definition of self-determination) than as a natural instantiation of self-determination rights. This shifts the reading''s character from coordination to something closer to ideology-enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the reading''s axioms are generic instantiations of liberal nationalism or reading-specific definitions smuggled in as neutral.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__liberal_nationalist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(jewi_tr_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(jewi_tr_t25, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 25, 0.24).
narrative_ontology:measurement(jewi_tr_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 40, 0.27).
narrative_ontology:measurement(jewi_tr_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 55, 0.28).
narrative_ontology:measurement(jewi_tr_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, theater_ratio, 75, 0.28).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(jewi_be_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(jewi_be_t25, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 25, 0.56).
narrative_ontology:measurement(jewi_be_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(jewi_be_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 55, 0.59).
narrative_ontology:measurement(jewi_be_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, base_extractiveness, 75, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jewi_su_t10, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 10, 0.38).
narrative_ontology:measurement(jewi_su_t25, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 25, 0.41).
narrative_ontology:measurement(jewi_su_t40, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 40, 0.42).
narrative_ontology:measurement(jewi_su_t55, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 55, 0.42).
narrative_ontology:measurement(jewi_su_t75, jewish_sovereignty_palestine__liberal_nationalist_reading, suppression_requirement, 75, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__liberal_nationalist_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_sovereignty_palestine__liberal_nationalist_reading, 0.12).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__post_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__liberal_nationalist_reading, jewish_sovereignty_palestine__cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% The kernel jewish_sovereignty_palestine admits five structurally distinct readings, each instantiating a different constraint with different ε values, beneficiary/victim sets, and classifications. The liberal_nationalist_reading frames the constraint as mutual self-determination requiring partition or binational coexistence (tangled_rope, ε≈0.58). The settler_colonial_reading reads the same kernel as instantiating displacement and demographic appropriation (snare, ε≈0.75+). The religious_zionist_reading reads the kernel as divine covenant and inalienable territorial claim (mountain or snare depending on grounding, ε near 0.0 or 0.8). The post_zionist_reading reads the constraint as achieving statehood but now obstructing civic equality (piton with mandatrophy, ε≈0.65 and rising theater_ratio). The cultural_zionist_reading reads the kernel as cultural renaissance without requiring political sovereignty or territorial dominance (rope or scaffold, ε≈0.20). All five readings share a referent (the question of Jewish collective status and territorial presence in Palestine) but author different ε values, different beneficiary/victim structures, and different structural classifications because they read the referent's structure differently. These are not measurement-basis ambiguities (observable-selection-dependent ε); they are reading-dependent ε values. Each story carries its own SI (spatial_scope, identity_lock, exit options) derivations. The five stories are linked via network.affects_constraints: each reading influences the others by providing alternative framings that compete for legitimacy in public and academic discourse.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_sovereignty_palestine__liberal_nationalist_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
