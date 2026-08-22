% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Passover Commemorative Arrangement as Survival-Competence Transmission (D5 Reading)
 *   domain: religious/ritual/collective_memory
 *
 * SUMMARY:
 *   This story instantiates the survival_competence_reading of the kernel
 *   catastrophe_memory_function: the claim that commemorative catastrophe
 *   ritual is survival technology — that the annual embodied rehearsal
 *   transmits adaptive capacity (identity maintenance under displacement,
 *   institutional continuity without territory, procedural knowledge of
 *   persistence) across generations without centralized infrastructure. The
 *   paradigm case is the rabbinic Passover seder: household-scale, portable,
 *   deliberately child-directed, constructed after the destruction of the
 *   Second Temple (70 CE) precisely when the community lost its centralized
 *   institutions; the arrangement's historical record — diaspora continuity
 *   through expulsions, persecutions, and the Holocaust aftermath — is the
 *   reading's standing evidence. EPSILON REFERENT: the standing Passover
 *   commemorative arrangement, assessed by this reading's own lights
 *   (functioning adaptive technology with real overhead), never the
 *   arrangement a sibling reading would endorse and never an idealized
 *   alternative. FAMILY DECOMPOSITION: the colloquial label 'the ritual
 *   preserves the memory of catastrophe' conflates structurally distinct
 *   claims; per the epsilon-invariance principle the kernel decomposes into
 *   three readings, each with its own epsilon, victim set, and classification
 *   — mourning_practice_reading and hybrid_transformation_reading are
 *   separate constraints linked via network.affects_constraints, and this
 *   story's epsilon would change if measured under their lights, which is
 *   exactly why it is not measured under their lights. CLAIM/METRIC
 *   INDEPENDENCE: claimed_type rope is this reading's honest structural claim
 *   (genuine coordination, net-beneficial participants, no active enforcement
 *   required); the metrics are authored descriptively and independently —
 *   modest extraction, decayed enforcement, low theater — and the engine
 *   computes per-seat classifications from the structural data without
 *   reference to the claim.
 *
 * KEY AGENTS:
 *   - diaspora_jewish_households: primary beneficiary and executing unit (moderate/identity_locked) — runs the annual rehearsal, bears its costs, receives the transmitted competence
 *   - rabbinic_authorities: agenda_setter and modest capture seat (institutional/identity_locked) — stewards the normative text, collects standing and role from the arrangement
 *   - seder_labor_bearers: primary cost-bearers (moderate/constrained) — disproportionate preparatory labor, historically without liturgical voice
 *   - children_in_transmission: transmission target (powerless/trapped) — compulsory rehearsal, primary recipient of transmitted competence
 *   - secular_and_apostate_members: excluded voice (moderate/arbitrage) — has exited or minimized practice; bears kinship exit costs; absent from norm-setting
 *   - ritual_transmission_scholars: analytical observer (analytical/analytical) — documents the transmission function and its drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.32).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Passover Commemorative Arrangement as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious/ritual/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'c5aefbb8-512e-433b-8e4b-be883d181c83').
narrative_ontology:cs_kernel_codification('c5aefbb8-512e-433b-8e4b-be883d181c83', fixed_text).
narrative_ontology:cs_authority_grounding('c5aefbb8-512e-433b-8e4b-be883d181c83', lineage).
narrative_ontology:cs_interpretation_layer_present('c5aefbb8-512e-433b-8e4b-be883d181c83').
narrative_ontology:cs_reading_relation('c5aefbb8-512e-433b-8e4b-be883d181c83', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('c5aefbb8-512e-433b-8e4b-be883d181c83', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('c5aefbb8-512e-433b-8e4b-be883d181c83', foundational, ritual_transmits_survival_competence).
narrative_ontology:cs_axiom_status(ritual_transmits_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('c5aefbb8-512e-433b-8e4b-be883d181c83', ritual_transmits_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('c5aefbb8-512e-433b-8e4b-be883d181c83', secondary, decentralized_continuity_requires_embodied_rehearsal).
narrative_ontology:cs_axiom_status(decentralized_continuity_requires_embodied_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('c5aefbb8-512e-433b-8e4b-be883d181c83', decentralized_continuity_requires_embodied_rehearsal, instrumental).
narrative_ontology:cs_reference_frame('c5aefbb8-512e-433b-8e4b-be883d181c83', adaptive_survival_technology).
narrative_ontology:cs_drift_state('c5aefbb8-512e-433b-8e4b-be883d181c83', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('c5aefbb8-512e-433b-8e4b-be883d181c83', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, seder_labor_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, seder_labor_bearers).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, children_in_transmission).
narrative_ontology:constraint_victim(catastrophe_memory_function__survival_competence_reading, children_in_transmission).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_survival_competence_hypothesis).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, decentralized_institutional_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Run the annual household rehearsal: prepare the meal, gather the extended family, retell the departure narrative with each participant positioned as having personally left, and direct the scripted questions to the children. They receive the transmitted package — narrative, procedures, continuity know-how — and they bear the preparation costs and the annual obligation. Leaving the practice would mean leaving the kin network and meaning-structure the practice organizes; for most that is not a live option.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, diaspora_jewish_households, beneficiary,
    moderate, generational, identity_locked, global).

% Adjudicate the ritual's normative questions — what counts as fulfillment, what may be changed, who is obligated — through responsa, haggadah editions, and communal guidance. Their standing, role, and livelihood are bound to the tradition they administer; revisiting its core is the one move their position cannot easily make. Historically they commanded communal sanction; today their authority rests on voluntary deference.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Carry the disproportionate preparatory work the annual rehearsal requires — the cleaning, cooking, and table labor that precedes the liturgy — and historically did so without proportional liturgical or decision-making voice in the ritual itself. In egalitarian and liberal communities this labor has been redistributed; in traditional communities the asymmetry persists. Exiting the labor role runs against household and communal expectation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, seder_labor_bearers, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, seder_labor_bearers, beneficiary).

% Are the rehearsal's designated audience: the scripted questions, the typology of the four children, and the pedagogical structure all aim at them. They bear compulsory attendance and the obligation frame before they can consent to it, and they receive the narrative, the procedures, and the identity the rehearsal transmits. They cannot exit; on reaching adulthood they choose the practice or leave it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, children_in_transmission, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, children_in_transmission, payer).

% Have exited full practice or minimized it to symbolic annual participation. They bear the kinship and identity costs of exit — strained family expectations, reduced standing in the community of origin — and they are absent from the norm-setting conversation even though its boundary decisions (who counts, intermarriage legitimacy) reach into their lives. Their departure is the live demonstration that the arrangement no longer commands enforcement.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, secular_and_apostate_members, excluded,
    moderate, biographical, arbitrage, global).

% Study the arrangement from outside: historians of rabbinic Judaism, ritual theorists, social scientists of collective memory. They document what the rehearsal transmits, how its enforcement decayed, and where practice has drifted into performance. They collect and produce analysis; they neither run the arrangement nor bear its obligations.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, ritual_transmission_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_function__survival_competence_reading, rabbinic_authorities).
narrative_ontology:fixing_cost_class(catastrophe_memory_function__survival_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the transgenerational transmission problem of a community with no centralized institution: how does a dispersed, stateless population pass catastrophe-survival competence — identity maintenance under displacement, institutional continuity without territory, procedural knowledge of persistence — to each new cohort? The annual synchronized household rehearsal solves it once per year, in every household, with the children as mandatory participants.
% TRANSFER_FUNCTION: Moves time, labor, and attention from community members into an annual transmission event — disproportionately preparatory labor from seder_labor_bearers — and moves procedural knowledge, narrative, and identity from the tradition's texts and elders to children. Historically it also moved deference and resources toward communal institutions; that transfer has decayed with enforcement.
% ABSENT_VOICES: Secular and apostate members are absent from the norm-setting conversation though its boundary decisions reach them; women historically absent from liturgical decision-making while bearing its labor; children present as audience but voiceless about the ritual's form. Their objections — voluntariness, labor equity, the psychological weight of the catastrophe frame — are registered only outside the structure that could act on them.
% DISAPPEARANCE_RATIONALE: The transmission channel would be gone: households would lose the annual synchronized rehearsal, the intergenerational handoff of survival procedure and identity would need replacement through schools or written curricula with no track record of carrying the same load, and communal time-structure would fragment. The historical comparison is direct — communities and family lines that abandoned the commemorative practice did not retain the competence package; assimilated descendent communities are the standing counterfactual.
% FOUNDING_PROBLEM: After the destruction of the Second Temple in 70 CE and the failure of the revolts, the community lost every centralized institution through which its identity and practice had been coordinated. The rabbinic project needed a mechanism that could preserve continuity, identity, and adaptive capacity under statelessness and dispersion — the household seder was constructed on older pilgrimage-rite foundations as the portable, replicable, child-directed answer.
% FOUNDING_PROBLEM_CORROBORATION: Historians of rabbinic Judaism — outside the beneficiary set — corroborate the founding problem itself: the post-70 institutional collapse and the household-scale reconstitution are standard scholarship. On current status, corroboration splits: demographers of Jewish continuity and secular continuity organizations (also outside the observant beneficiary set) treat discontinuity and assimilation as live problems and act on that reading; historians of the modern period note that sovereignty and integration have retired parts of the original problem. No outside party attests the problem is simply dead; none attests it is simply live.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.32: the standing arrangement's costs — the gendered preparatory-labor asymmetry, compulsory childhood rehearsal, the obligation frame's psychological weight — are real but modest against the transmitted benefit; this reading assesses the arrangement as net-beneficial technology with overhead, not rent collection. Suppression 0.28 is authored raw and unscaled (the engine scales only extractiveness): the arrangement today holds by socialization and identity rather than force; the historical enforcement machinery (communal discipline, excommunication) has decayed, and the suppression_requirement series documents that decay from a kehillah-autonomy peak of 0.62 to 0.28. Theater 0.22: the rehearsal's activity is predominantly its function — the performance IS the transmission — with a rote residue from liturgical accretion and comprehension gaps (see the transmission_vs_rote_decay omega). Accessibility_collapse 0.35: alternatives to ritual transmission (schools, written curricula, museums) exist and function, but community norms channel transmission through the annual rehearsal, so alternatives are partly foreclosed rather than collapsed. Resistance 0.30: reform liturgies, feminist and egalitarian revisions, and secular exit are real but marginal to the core. requires_active_enforcement is false: persistence is socialization-based; nothing coercive holds the arrangement up — which is also why removal is cheap. Atrophy, already visible in secularized segments, is the removal path, and the labor asymmetry has been removed at modest cost where communities chose to remove it. The three metric series share one time grid (0/300/600/900/1200/1500/1820, approximately 200 CE to the 2020s) so every metric is authored at every examined point; the extractiveness hump tracks the persecution eras (exit closed, deviation costly) rather than a cycle — the oscillation is externally driven, not an intermittent-reinforcement mechanism. Boltzmann: identity_coordination reflects the genuine boundary-maintenance and continuity function; the coupling check finds no Power-by-Scope concentration of extraction — the labor asymmetry is intra-communal, not scope-amplified against the powerless.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the rabbinic_authorities seat the arrangement is the community's survival technology, stewarded across two millennia — low extraction, high function. From the seder_labor_bearers seat the same arrangement is an obligation structure whose costs fall unevenly and whose liturgical voice was historically closed to them — the extraction they experience is what the victims declaration records. From the secular_and_apostate_members seat it is a voluntary heritage with kinship exit costs. Identity-lock dynamics: households and authorities are identity_locked by relational and institutional fusion — the household's annual practice IS the community's continuity, and the authority's standing IS the tradition; if that identity frame breaks (as it partially has in secularized segments), exit opens, which is precisely what the modern suppression decline measures. The engine computes these per-seat classifications from the structural data; the authored rope claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: diaspora_jewish_households (receive the transmitted competence; identity-locked, so their d sits low-moderate rather than at the pure-beneficiary end — they also bear real costs) and rabbinic_authorities (collect standing and role; low d; the receipt surface names them the modest capture seat — positional goods, not material rents). Victims: seder_labor_bearers (bear the asymmetric preparatory labor under constrained exit; high d). children_in_transmission are primary beneficiaries (the mechanism's whole point) with a secondary payer position (compulsory rehearsal before consent is possible) — the derivation should place them near-beneficiary. secular_and_apostate_members have exercised arbitrage-grade exit; the arrangement no longer extracts from them, so their d sits near the beneficiary end despite exclusion from norm-setting. No directionality_overrides are used: overrides key on power_atom, and this story's moderate-power agents span beneficiary (households), payer (labor bearers), and excluded (secular members) relationships — a per-atom override would misapply to two of the three. The role-plus-exit derivation is the correct input here.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (continuity under statelessness after 70 CE) is contested rather than dead: sovereignty and integration have retired parts of it, while diaspora continuity and recurring persecution keep the competence question live, with outside corroboration on both sides. The arrangement has not atrophied into performance: its transmission function demonstrably operated across the interval — the community persisted through every catastrophe the rehearsal encodes. The piton pathway is nonetheless visible at the margins and tracked by the transmission_vs_rote_decay omega: if recitation decouples from comprehension in liberal segments, theater_ratio rises there and those segments drift toward theatrical maintenance. Mandatrophy discipline cuts both ways: reading the labor asymmetry as pure extraction would erase the genuine coordination function the historical record attests; reading the arrangement as costless coordination would erase the real, unevenly borne costs the victims declaration records. The mismatch consumer reads founding_problem_status=contested against disappearance_verdict=world_rearranges — no dead-problem-plus-rearranging-world capture flag fires, consistent with the rope claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_epsilon_indexing,
    'This constraint is the survival_competence_reading of kernel catastrophe_memory_function: what epsilon and victim set would the mourning_practice_reading and hybrid_transformation_reading author for the same standing arrangement, and would their computed classifications diverge from this reading''s rope?',
    'Author the sibling stories and compare computed per-seat classifications over the shared referent; divergence localizes the disagreement to what the ritual''s structure actually preserves rather than to measurement error.',
    'If siblings compute tangled_rope or snare where this reading computes rope, the kernel''s classification is reading-relative and the hybrid reading''s both-functions claim gains structural support; if all three compute rope, the disagreement is evaluative, not structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_indexing, conceptual, 'Reading-indexed classification over a shared kernel referent.').

omega_variable(
    transmission_vs_rote_decay,
    'What share of contemporary ritual activity is load-bearing transmission versus rote performance (recitation without comprehension), and does the share differ between observant and liberal communities?',
    'Comprehension and ethnographic studies of seder practice across community types; longitudinal retention data on transmitted knowledge in participating cohorts.',
    'A rising rote share would raise theater_ratio and mark a piton pathway in liberal segments; a stable low share supports the rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transmission_vs_rote_decay, empirical, 'Rote-performance share of ritual activity.').

omega_variable(
    labor_asymmetry_separability,
    'Is the gendered preparatory-labor asymmetry intrinsic to the transmission function, or a separable customary accretion the function does not require?',
    'Comparative study of communities that redistributed the labor (egalitarian and feminist seder practice): if transmission outcomes hold with redistributed labor, the asymmetry is separable.',
    'If separable, the constraint is a rope with a removable extractive rider; if intrinsic, the standing arrangement is a tangled_rope and this reading''s epsilon understates extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_asymmetry_separability, empirical, 'Whether the labor asymmetry is part of the constraint''s function.').

omega_variable(
    persecution_frame_net_effect,
    'Does the ritual''s catastrophe frame (''in every generation they rise against us'') impose net psychological and social costs (threat hypervigilance, out-group distrust) that count as extraction, or does it function as adaptive threat-preparedness?',
    'Social-psychological studies of commemorative framing effects on threat perception and out-group trust across participating cohorts.',
    'Net cost would raise this reading''s epsilon; net adaptive effect would strengthen the low-epsilon claim and the D5 thesis itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_frame_net_effect, empirical, 'Adaptive-versus-cost status of the embedded catastrophe frame.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is contemporary participation obligation structural (kin and community sanction) or internalized (identity obligation that would persist if sanctions were removed)?',
    'Exit-trajectory studies of secularization: if felt obligation persists after structural sanctions are gone (as in secularized descendants who nonetheless maintain seder practice), suppression is substantially internalized.',
    'If internalized, effective suppression exceeds the structural measure and exit remains costlier than it appears; if structural, further sanction decay converts the arrangement to fully voluntary participation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism of contemporary participation obligation.').

omega_variable(
    function_separability_d1_d5,
    'Can the survival-competence transmission function be separated from the mourning/boundary function in the ritual''s actual structure, or do they necessarily co-occur (the hybrid reading''s claim)?',
    'Comparative ritual analysis of transmission without the memorial frame (secular seders, freedom seders, museum pedagogy): if competence transmission holds without the mourning frame, the functions are separable.',
    'If separable, this reading''s constraint is cleanly epsilon-invariant; if inseparable, this reading''s epsilon is confounded with the mourning function''s costs and the hybrid reading is structurally correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(function_separability_d1_d5, conceptual, 'Whether D5 transmission is separable from D1/D4 preservation in the ritual structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 1820).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t300, catastrophe_memory_function__survival_competence_reading, theater_ratio, 300, 0.17).
narrative_ontology:measurement_basis(cata_tr_t300, observed).
narrative_ontology:measurement(cata_tr_t600, catastrophe_memory_function__survival_competence_reading, theater_ratio, 600, 0.2).
narrative_ontology:measurement_basis(cata_tr_t600, observed).
narrative_ontology:measurement(cata_tr_t900, catastrophe_memory_function__survival_competence_reading, theater_ratio, 900, 0.24).
narrative_ontology:measurement_basis(cata_tr_t900, observed).
narrative_ontology:measurement(cata_tr_t1200, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1200, 0.27).
narrative_ontology:measurement_basis(cata_tr_t1200, observed).
narrative_ontology:measurement(cata_tr_t1500, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1500, 0.25).
narrative_ontology:measurement_basis(cata_tr_t1500, observed).
narrative_ontology:measurement(cata_tr_t1820, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1820, 0.22).
narrative_ontology:measurement_basis(cata_tr_t1820, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t300, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 300, 0.4).
narrative_ontology:measurement_basis(cata_be_t300, observed).
narrative_ontology:measurement(cata_be_t600, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 600, 0.41).
narrative_ontology:measurement_basis(cata_be_t600, observed).
narrative_ontology:measurement(cata_be_t900, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 900, 0.44).
narrative_ontology:measurement_basis(cata_be_t900, observed).
narrative_ontology:measurement(cata_be_t1200, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1200, 0.47).
narrative_ontology:measurement_basis(cata_be_t1200, observed).
narrative_ontology:measurement(cata_be_t1500, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1500, 0.43).
narrative_ontology:measurement_basis(cata_be_t1500, observed).
narrative_ontology:measurement(cata_be_t1820, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1820, 0.32).
narrative_ontology:measurement_basis(cata_be_t1820, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t300, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 300, 0.52).
narrative_ontology:measurement_basis(cata_su_t300, observed).
narrative_ontology:measurement(cata_su_t600, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 600, 0.55).
narrative_ontology:measurement_basis(cata_su_t600, observed).
narrative_ontology:measurement(cata_su_t900, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 900, 0.62).
narrative_ontology:measurement_basis(cata_su_t900, observed).
narrative_ontology:measurement(cata_su_t1200, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1200, 0.58).
narrative_ontology:measurement_basis(cata_su_t1200, observed).
narrative_ontology:measurement(cata_su_t1500, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1500, 0.45).
narrative_ontology:measurement_basis(cata_su_t1500, observed).
narrative_ontology:measurement(cata_su_t1820, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1820, 0.28).
narrative_ontology:measurement_basis(cata_su_t1820, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Passover commemorates the Exodus' conflates structurally distinct claims about what the ritual preserves. Per the epsilon-invariance principle, the kernel catastrophe_memory_function decomposes into three readings, each with its own epsilon, victim set, and classification: mourning_practice_reading (memorial obligation and boundary-norms; higher obligation costs), survival_competence_reading (this story: adaptive-capacity transmission; low-moderate epsilon, rope claim), and hybrid_transformation_reading (both encoded; intermediate). Upstream/downstream structure: documented transmission outcomes — this reading's evidence base — are the material the hybrid reading incorporates, while the mourning reading stands as a rival functionalist account coexisting with both. Each story links the other two via affects_constraints; the confusion lives in the shared label, not in the structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
