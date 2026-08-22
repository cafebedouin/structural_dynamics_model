% ============================================================================
% CONSTRAINT STORY: script_as_identity__kemalist_rupture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_script_as_identity__kemalist_rupture_reading, []).

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
 *   constraint_id: script_as_identity__kemalist_rupture_reading
 *   human_readable: Latin Script Mandate as State Monopoly on Secular Literacy (Kemalist Rupture Reading)
 *   domain: political/linguistic/identity
 *
 * SUMMARY:
 *   Between 1923 and 1928, the Turkish state under Atatürk mandated the
 *   replacement of Arabic script with Latin script for all official uses,
 *   public education, and published media. The Kemalist reading frames this
 *   as a necessary rupture enabling secular modernization: Arabic script
 *   binds Turkish to Islamic identity and Ottoman tradition; Latin script
 *   opens access to European knowledge and signals alignment with secular
 *   modernity. This reading treats the script change as a feature—textual
 *   discontinuity is valued as ideologically productive, severing the
 *   population from pre-modern authority structures. The constraint operates
 *   as enforced coordination (unified literacy apparatus) layered over
 *   asymmetric extraction (rupture of Islamic scholarly tradition,
 *   identity-lock of rural populations into the state's new literacy
 *   monopoly). The claim/metric gap is intentional: this reading CLAIMS
 *   rope-type coordination while the metrics show substantial extraction,
 *   active suppression, and rising theater (the coordination function
 *   increasingly becomes window-dressing for cultural domination). The
 *   measurement series track the period from the founding of the Turkish
 *   Republic through the First Democratic Election, capturing the enforcement
 *   ramp-up (1928 mandate → 1935 peak suppression) and slight decay in
 *   suppression as cohorts shift (but rising theater as the mandate
 *   normalizes and its extractive function becomes less visible).
 *
 * KEY AGENTS:
 *   - secular_state_apparatus — institutional agenda-setter; monopolizes literacy apparatus; enforces the mandate through schools and public signage
 *   - modernist_intellectual_class — organized beneficiary; gains authority as fluent in Latin script and European texts; frames rupture as inevitable progress
 *   - ottoman_islamic_textual_communities — powerless payers; identity-locked into pre-modern literacy; cannot access Ottoman-Islamic scholarship without learning Arabic script anew
 *   - religious_scholars — moderate-power payers with constrained exits; transmission lineage disrupted; excluded from public education and state discourse
 *   - rural_populations — powerless trapped payers; face rapid literacy obsolescence and uneven access to new schools
 *   - ottoman_past_as_constitutive — non-agent symbolic victim; the archive of Ottoman administrative records and Islamic scholarship is rendered inaccessible by script rupture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, 0.68).
domain_priors:suppression_score(script_as_identity__kemalist_rupture_reading, 0.71).
domain_priors:theater_ratio(script_as_identity__kemalist_rupture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(script_as_identity__kemalist_rupture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(script_as_identity__kemalist_rupture_reading, tangled_rope).
narrative_ontology:human_readable(script_as_identity__kemalist_rupture_reading, "Latin Script Mandate as State Monopoly on Secular Literacy (Kemalist Rupture Reading)").
narrative_ontology:topic_domain(script_as_identity__kemalist_rupture_reading, "political/linguistic/identity").

domain_priors:requires_active_enforcement(script_as_identity__kemalist_rupture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(script_as_identity__kemalist_rupture_reading, '27768a38-190a-4c21-8c9b-5c57643467d5').
narrative_ontology:cs_kernel_codification('27768a38-190a-4c21-8c9b-5c57643467d5', fixed_text).
narrative_ontology:cs_authority_grounding('27768a38-190a-4c21-8c9b-5c57643467d5', extraction).
narrative_ontology:cs_interpretation_layer_present('27768a38-190a-4c21-8c9b-5c57643467d5').
narrative_ontology:cs_reading_relation('27768a38-190a-4c21-8c9b-5c57643467d5', script_as_identity__ottoman_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('27768a38-190a-4c21-8c9b-5c57643467d5', script_as_identity__phonetic_instrumentalism_reading, coexists_with).
narrative_ontology:cs_axiom('27768a38-190a-4c21-8c9b-5c57643467d5', foundational, rupture_enables_secularization).
narrative_ontology:cs_axiom_status(rupture_enables_secularization, holdable).
narrative_ontology:cs_axiom_grounding('27768a38-190a-4c21-8c9b-5c57643467d5', rupture_enables_secularization, deontological).
narrative_ontology:cs_axiom('27768a38-190a-4c21-8c9b-5c57643467d5', foundational, european_script_signals_modernity).
narrative_ontology:cs_axiom_status(european_script_signals_modernity, holdable).
narrative_ontology:cs_axiom_grounding('27768a38-190a-4c21-8c9b-5c57643467d5', european_script_signals_modernity, conventional).
narrative_ontology:cs_axiom('27768a38-190a-4c21-8c9b-5c57643467d5', secondary, arabic_script_constitutively_islamic).
narrative_ontology:cs_axiom_status(arabic_script_constitutively_islamic, holdable).
narrative_ontology:cs_axiom_grounding('27768a38-190a-4c21-8c9b-5c57643467d5', arabic_script_constitutively_islamic, empirically_contingent).
narrative_ontology:cs_reference_frame('27768a38-190a-4c21-8c9b-5c57643467d5', ottoman_islamic_textual_authority).
narrative_ontology:cs_drift_state('27768a38-190a-4c21-8c9b-5c57643467d5', kemalist_rupture_1928, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('27768a38-190a-4c21-8c9b-5c57643467d5', '').
narrative_ontology:cs_kernel_id(script_as_identity__kemalist_rupture_reading, script_as_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, secular_state_apparatus).
narrative_ontology:constraint_beneficiary(script_as_identity__kemalist_rupture_reading, modernist_intellectual_class).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, ottoman_islamic_textual_communities).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, religious_scholars).
narrative_ontology:constraint_victim(script_as_identity__kemalist_rupture_reading, rural_populations).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, secularism_requires_textual_break).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, arabic_script_binds_islamic_identity).
narrative_ontology:constraint_vindicates(script_as_identity__kemalist_rupture_reading, state_monopoly_on_literacy_enables_nation_building).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the mandate in 1928, enforces through educational monopoly and printing regulations, maintains through textbook standardization and public signage. Justifies as modernization necessity. Collects authority over national identity and literacy apparatus.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, secular_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Gains prestige as fluent interpreters of European modernity; their career advancement depends on Latin-script expertise and distance from Islamic scholarship. Becomes the certified intellectual class of the new state.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, modernist_intellectual_class, beneficiary,
    organized, biographical, mobile, national).

% Loses direct access to Ottoman-Islamic scholarly tradition. Cannot read Qur'an, fiqh, Ottoman administrative records without learning Arabic script separately. Faces institutional exclusion from education and public discourse.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, ottoman_islamic_textual_communities, payer,
    powerless, biographical, identity_locked, national).

% Transmission lineage disrupted by cohort of students who cannot read pre-modern texts. Systematically excluded from state education and public intellectual forums. Resistance dismissed as reactionary.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, religious_scholars, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(script_as_identity__kemalist_rupture_reading, religious_scholars, excluded).

% Face rapid literacy obsolescence; those literate in Arabic script cannot transfer skill; new literacy requires state schools. Uneven school distribution in rural areas creates structural inequality in transition cost.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, rural_populations, payer,
    powerless, biographical, trapped, national).

% Design and enforce curriculum, textbook standards, printing house regulations. Implement the mandate and measure compliance through literacy cohorts. Technical role embedded in political project.
narrative_ontology:constraint_stakeholder(script_as_identity__kemalist_rupture_reading, literacy_planning_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(script_as_identity__kemalist_rupture_reading, secular_state_apparatus).
narrative_ontology:fixing_cost_class(script_as_identity__kemalist_rupture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified state literacy apparatus by severing pre-modern textual fragmentation: a single script enables centralized education, standardized bureaucracy, and homogeneous national consciousness. Before the mandate, literacy was stratified (religious scholars in Arabic, merchants in various scripts, bureaucrats in Ottoman Turkish). The Latin script mandate consolidates literacy under state control and creates a single gateway to modern knowledge.
% TRANSFER_FUNCTION: Transfers interpretive authority from Ottoman-Islamic scholarly communities (fuqaha, ulama, Sufi masters) to the secular state and its educational apparatus. Transfers historical continuity (access to Ottoman records and Islamic scholarship) from all populations to a privileged minority fluent in both scripts. Transfers the definition of legitimate knowledge from religious and traditional sources to European scientific and secular ones.
% ABSENT_VOICES: Ottoman-Islamic scholars whose transmission lineages depend on reading fluency are systematically excluded from designing or contesting the mandate. Rural populations and the urban poor, who carry oral knowledge in environments where literacy is transitional, cannot effectively voice the disruption costs. The reading's own sibling readings (ottoman_continuity_reading, phonetic_instrumentalism_reading) represent suppressed alternative interpretations of the kernel, present in Turkish intellectual discourse but excluded from policy design.
% DISAPPEARANCE_RATIONALE: If the Latin script mandate had not been enforced, Ottoman-Islamic literacy would have persisted as a competing parallel system; Turkish would likely have retained Arabic script (as did Persian, Urdu, and Arabic itself); the continuity of pre-modern scholarly tradition would have remained accessible; religious authority would not have undergone structural disruption; Turkish national identity would be framed as Islamic-modern rather than secular-European. The mandate is not neutral—its removal would restore the institutional landscape it severed.
% FOUNDING_PROBLEM: Rapid modernization requires unified literacy and access to European scientific knowledge. Ottoman administrators and intellectuals perceived Arabic script as slow to teach, as binding Turkish to Islamic tradition (blocking European alignment), and as fragmentary across dialects and regions. The founding problem, as this reading frames it, is: How can a state rapidly transform its population into secular citizens with access to modern (European) knowledge?
% FOUNDING_PROBLEM_CORROBORATION: Kemalist state officials and modernist intellectuals (Atatürk, linguistic reformers, education planners) attest the founding problem remains live: Arabic script is seen as perpetuating Islamic identity and slowing literacy diffusion. Ottoman-Islamic scholars and conservative opponents attest the problem is a constructed one, serving ideological rupture rather than genuine functional necessity. International observers (some European linguists in the 1920s–1930s) supported the modernist reading; contemporary historical analysis increasingly questions whether the speed of literacy transition justified the cultural rupture.
narrative_ontology:disappearance_verdict(script_as_identity__kemalist_rupture_reading, world_rearranges).
narrative_ontology:founding_problem_status(script_as_identity__kemalist_rupture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(script_as_identity__kemalist_rupture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(script_as_identity__kemalist_rupture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(script_as_identity__kemalist_rupture_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(script_as_identity__kemalist_rupture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(script_as_identity__kemalist_rupture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(script_as_identity__kemalist_rupture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the constraint transfers interpretive authority from Islamic scholars to the secular state and redefines legitimate knowledge as European-aligned. It extracts historical continuity and identity resources from populations whose authority derives from pre-modern texts. Suppression is similarly high (0.71) and rises sharply at 1928 (the mandate date) because the state actively blocks Arabic-script publication, removes Arabic-literate teachers from schools, and criminalizes or marginalizes Arabic-script use in public. Theater ratio (0.42) reflects the dual nature: genuine coordination gains from unified literacy (reducing fragmentation) but increasingly serving political domination rather than functional education as the system matures. Accessibility collapse is high (0.79) because alternatives (retaining Arabic script, maintaining parallel systems) are systematically excluded by state monopoly on schooling and publishing. Resistance is moderate (0.58) because the payee populations (Islamic scholars, rural populations) lack organizational capacity to mount sustained opposition within the constraints of an authoritarian state; resistance takes the form of private Qur'anic study circles and oral knowledge transmission, which the mandate can suppress but not fully eliminate. The measurement series show extractiveness rising from 0.45 (pre-mandate) to 0.68 (stabilized), with suppression spiking at the 1928 mandate and remaining high; theater is lowest at the start (minimal institutional cover needed) and rises as the mandate normalizes and the state articulates increasingly elaborate modernization rationales.
 *
 * PERSPECTIVAL GAP:
 *   From the state apparatus seat, the arrangement is genuine coordination—unified literacy enables efficient education, reduces administrative fragmentation, and signals alignment with modernity. From the religious scholar seat, the same structure is identity erasure and institutional domination: their transmission authority is severed, their texts rendered inaccessible to new generations, their role redefined as backward obscurantism. From the rural population seat, it is rapid obsolescence and unequal access: those literate in Arabic cannot transfer the skill; those not yet literate must learn the new system from fewer schools. From the modernist intellectual seat, it is liberation and prestige: the rupture vindicates their cosmopolitan worldview and makes them the gatekeepers of legitimate knowledge. The engine computes each seat's experienced type from power, exit options, beneficiary/victim status, and measurement data; the structural asymmetry is not reconciled here.
 *
 * DIRECTIONALITY LOGIC:
 *   The secular_state_apparatus is a clear institutional beneficiary (d near 0.1): it collects authority and control through the monopoly on literacy. The modernist_intellectual_class benefits through prestige and authority (d ≈ 0.2): they become the certified interpreters of modernity. Ottoman_islamic_textual_communities are clear targets (d ≈ 0.9): they pay through identity rupture and loss of transmission authority; their exit is identity-locked—leaving Turkish-Islamic identity to escape the constraint is incoherent. Religious_scholars are also targets (d ≈ 0.85): their power (moderate) allows some resistance, but their exit is constrained by professional identity tied to pre-modern texts. Rural_populations are trapped targets (d ≈ 0.95): powerless, with no mobile exit and facing literacy obsolescence. The directionality overrides below correct for the fact that moderate-power religious_scholars might superficially appear symmetric (organizational capacity, biographical time horizon) but are structurally targets due to identity-lock and constrained exits.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (enable rapid modern literacy and secular identity) diverges from its effective function (state monopoly on cultural authority and rupture of Islamic scholarly tradition). The founding problem—rapid access to modern knowledge—could plausibly be solved by teaching both Arabic and Latin scripts (many multilingual societies do so), making the enforcement of script exclusivity not mandated by the founding problem itself but by the ideological goal of severing Ottoman-Islamic identity. The theater ratio rises over time (0.15 → 0.42) as the practical benefits of unified literacy stabilize but enforcement focus shifts to symbolic dominance: by 1942, the constraint is increasingly maintained through institutional inertia and public education textbook standardization rather than active suppression. A true mandatrophy diagnosis would compare the cost of continued enforcement against the founding problem's residual salience; by 1950, the founding problem (literacy rate, access to modern knowledge) was substantially solved, yet the script monopoly persists and requires ongoing enforcement through educational curricula.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_rupture_functional_necessity,
    'Was script exclusivity functionally necessary to achieve rapid literacy gains, or would multilingual literacy (both scripts) have achieved the same educational outcomes with less cultural rupture?',
    'Comparative historical analysis: literacy rates in multilingual script-transition countries (e.g., Vietnam romanization, India''s Devanagari adoption) vs. exclusive-script transitions; controlled measurement of reading speed and comprehension across script systems.',
    'If multilingual outcomes were achievable, the constraint''s extraction is substantially unjustified; if exclusive literacy achieved materially faster transition, the extraction is a cost of functional necessity. This determines whether the constraint is Tangled Rope (coordination + extraction) or Snare (extraction with coordination as cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_rupture_functional_necessity, empirical, 'Whether script exclusivity was necessary or ideologically chosen for rapid modernization.').

omega_variable(
    reading_foreclosure_boundary,
    'Does the Kemalist rupture reading logically foreclose the ottoman_continuity reading within a single state framework, or are these genuinely coexisting political positions?',
    'Textual analysis of Kemalist ideology and counter-positions in Ottoman-Islamic conservative thought; check whether either tradition permits holding both ''rupture is necessary'' and ''continuity is constitutive'' without contradiction.',
    'If genuine foreclosure, the constraint represents a zero-sum identity contest; if coexistence, it represents political suppression of a live alternative reading. This affects the classification of the sibling reading (ottoman_continuity_reading) and the structure of the constraint family.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether the Kemalist and ottoman_continuity readings can coexist in single framework or logically foreclose each other.').

omega_variable(
    identity_lock_mechanism_internalization,
    'Is the suppression of Ottoman-Islamic textual identity primarily structural (external barriers: banned books, no schools, state monopoly on printing) or internalized (populations internalize the secular frame and come to see Arabic script as backward)?',
    'Post-1950 trajectory analysis: if suppression persists after state enforcement loosens (allowing private Arabic-script publishing, optional Qur''anic schools), reclassify as partially internalized. If suppression decays rapidly, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the 0.71 metric suggests; the target population carries identity rupture with them even after external barriers lift. If structural, relaxed enforcement should permit recovery of Ottoman-Islamic literacy practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_internalization, empirical, 'Structural vs. internalized suppression of Ottoman-Islamic textual identity.').

omega_variable(
    modernist_intellectual_benignity,
    'Is the modernist_intellectual_class a genuine beneficiary collecting rents from the constraint, or are they captured agents executing a state mandate they do not fully endorse?',
    'Archival analysis of modernist intellectuals'' private statements vs. public positions; career incentive structure; whether careers advanced fastest for constraint-enforcement advocates or for universal-education advocates willing to work with Arabic-script populations.',
    'If genuinely capturing rents, modernist intellectuals are structural beneficiaries and the constraint is maintained by coalition. If captured, the constraint is primarily state-enforced with intellectual cover; removing state enforcement might dissolve the beneficiary coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernist_intellectual_benignity, empirical, 'Whether modernist intellectuals are rent-capturing beneficiaries or captured executers of state ideology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(script_as_identity__kemalist_rupture_reading, 1923, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(script_kemalist_tr_t1923, script_as_identity__kemalist_rupture_reading, theater_ratio, 1923, 0.15).
narrative_ontology:measurement_basis(script_kemalist_tr_t1923, projected).
narrative_ontology:measurement(script_kemalist_tr_t1928, script_as_identity__kemalist_rupture_reading, theater_ratio, 1928, 0.35).
narrative_ontology:measurement_basis(script_kemalist_tr_t1928, observed).
narrative_ontology:measurement(script_kemalist_tr_t1935, script_as_identity__kemalist_rupture_reading, theater_ratio, 1935, 0.42).
narrative_ontology:measurement_basis(script_kemalist_tr_t1935, observed).
narrative_ontology:measurement(script_kemalist_tr_t1942, script_as_identity__kemalist_rupture_reading, theater_ratio, 1942, 0.44).
narrative_ontology:measurement_basis(script_kemalist_tr_t1942, observed).
narrative_ontology:measurement(script_kemalist_tr_t1950, script_as_identity__kemalist_rupture_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement_basis(script_kemalist_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(script_kemalist_be_t1923, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1923, 0.45).
narrative_ontology:measurement_basis(script_kemalist_be_t1923, projected).
narrative_ontology:measurement(script_kemalist_be_t1928, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1928, 0.62).
narrative_ontology:measurement_basis(script_kemalist_be_t1928, observed).
narrative_ontology:measurement(script_kemalist_be_t1935, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1935, 0.68).
narrative_ontology:measurement_basis(script_kemalist_be_t1935, observed).
narrative_ontology:measurement(script_kemalist_be_t1942, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1942, 0.7).
narrative_ontology:measurement_basis(script_kemalist_be_t1942, observed).
narrative_ontology:measurement(script_kemalist_be_t1950, script_as_identity__kemalist_rupture_reading, base_extractiveness, 1950, 0.68).
narrative_ontology:measurement_basis(script_kemalist_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(script_kemalist_su_t1923, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1923, 0.38).
narrative_ontology:measurement_basis(script_kemalist_su_t1923, projected).
narrative_ontology:measurement(script_kemalist_su_t1928, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1928, 0.65).
narrative_ontology:measurement_basis(script_kemalist_su_t1928, observed).
narrative_ontology:measurement(script_kemalist_su_t1935, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement_basis(script_kemalist_su_t1935, observed).
narrative_ontology:measurement(script_kemalist_su_t1942, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1942, 0.73).
narrative_ontology:measurement_basis(script_kemalist_su_t1942, observed).
narrative_ontology:measurement(script_kemalist_su_t1950, script_as_identity__kemalist_rupture_reading, suppression_requirement, 1950, 0.71).
narrative_ontology:measurement_basis(script_kemalist_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(script_as_identity__kemalist_rupture_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(script_as_identity__kemalist_rupture_reading, 0.12).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__ottoman_continuity_reading).
narrative_ontology:affects_constraint(script_as_identity__kemalist_rupture_reading, script_as_identity__phonetic_instrumentalism_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel script_as_identity. The sibling reading constraint_script_as_identity__ottoman_continuity_reading represents the claim that Arabic script is constitutive of Turkish-Islamic identity; it shares the same structural event (script mandate 1928) but assigns opposite directionality and function (continuity vs. rupture). The third sibling, constraint_script_as_identity__phonetic_instrumentalism_reading, treats script as neutral technology and locates the functional gain in phonetic fit rather than identity rupture. Each reading has its own epsilon, beneficiary/victim structure, and temporal trajectory. Links between siblings run through network.affects_constraints; they are not folded into a single story per the epsilon-invariance principle (OQ-DP-001).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(script_as_identity__kemalist_rupture_reading, moderate, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
