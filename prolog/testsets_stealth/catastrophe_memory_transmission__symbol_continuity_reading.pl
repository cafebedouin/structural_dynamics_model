% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__symbol_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__symbol_continuity_reading, []).

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
 *   constraint_id: catastrophe_memory_transmission__symbol_continuity_reading
 *   human_readable: Post-Catastrophe Ritual Fidelity Regime — Symbol-Continuity Reading
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   A community that survived catastrophic destruction — mass death,
 *   displacement, loss of territory and institutions — organizes its
 *   collective life around exact transmission of inherited ritual form:
 *   liturgy in the ancestral language, fixed mourning rites, a commemorative
 *   calendar anchored to the catastrophe. The symbol-continuity reading
 *   instantiated here holds that the forms are an intrinsic communal good and
 *   that their intergenerational transmission IS the community's survival
 *   mechanism; fidelity is therefore not one value among others but the
 *   load-bearing wall. The standing arrangement under contest — and the ε
 *   referent — is that fidelity-centered regime as this reading assesses it:
 *   a real coordination achievement that simultaneously forecloses
 *   adaptation, with costs falling asymmetrically on those who inherited the
 *   forms rather than authored them. Per the claim/metric independence rule,
 *   claimed_type records this reading's structural verdict (tangled_rope:
 *   genuine identity coordination fused with asymmetric extraction) while the
 *   metrics record descriptive operation; the engine computes per-seat types
 *   from the structural data, and any divergence between claim and
 *   computation is signal, not error.
 *
 * KEY AGENTS:
 *   - liturgical_council: agenda_setter (institutional/identity_locked) — administers the ritual calendar, certifies officiants, rules on permissible variation; authority and livelihood constituted by the forms it preserves
 *   - survivor_generation: primary beneficiary with payer overlay (organized/identity_locked) — the forms carry their dead; police deviation as loyalty; their living testimony is subordinated to liturgical form
 *   - younger_membership: primary payer with beneficiary overlay (moderate/constrained) — inherit forms without the memories; compliance consumes time, language, and choice; exit means losing the community
 *   - reform_advocates: excluded (moderate/trapped) — propose adapted vernacular forms; sanctioned as assimilationists; remain because exit surrenders the community they seek to repair
 *   - memory_scholars: analytical observer (analytical/analytical) — comparative record of post-catastrophe communities with and without portable ritual carriers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, 0.58).
domain_priors:suppression_score(catastrophe_memory_transmission__symbol_continuity_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_transmission__symbol_continuity_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__symbol_continuity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__symbol_continuity_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__symbol_continuity_reading, "Post-Catastrophe Ritual Fidelity Regime — Symbol-Continuity Reading").
narrative_ontology:topic_domain(catastrophe_memory_transmission__symbol_continuity_reading, "religious_studies/collective_memory/ritual_studies").

domain_priors:requires_active_enforcement(catastrophe_memory_transmission__symbol_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__symbol_continuity_reading, '21efd176-ca7f-4032-97c7-fcbcd4d3fd91').
narrative_ontology:cs_kernel_codification('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', fixed_text).
narrative_ontology:cs_authority_grounding('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', lineage).
narrative_ontology:cs_interpretation_layer_present('21efd176-ca7f-4032-97c7-fcbcd4d3fd91').
narrative_ontology:cs_reading_relation('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', catastrophe_memory_transmission__operational_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', foundational, symbolic_form_intrinsically_constitutive).
narrative_ontology:cs_axiom_status(symbolic_form_intrinsically_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', symbolic_form_intrinsically_constitutive, deontological).
narrative_ontology:cs_axiom('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', foundational, form_transmission_is_survival_mechanism).
narrative_ontology:cs_axiom_status(form_transmission_is_survival_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', form_transmission_is_survival_mechanism, empirically_contingent).
narrative_ontology:cs_reference_frame('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', ancestral_form_fidelity_baseline).
narrative_ontology:cs_drift_state('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', third_generation_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('21efd176-ca7f-4032-97c7-fcbcd4d3fd91', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, survivor_generation).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, liturgical_council).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, younger_membership).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, reform_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__symbol_continuity_reading, younger_membership).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__symbol_continuity_reading, survivor_generation).
narrative_ontology:constraint_vindicates(catastrophe_memory_transmission__symbol_continuity_reading, symbolic_form_survival_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the ritual calendar, certifies who may officiate, and rules on how much variation the inherited forms tolerate. Its members' standing, livelihood, and daily schedule are constituted by the requirement that the forms be kept: endowment income flows to offices that exist only because transmission is mandatory, and certification power decides who earns a living inside the community's religious economy. Stepping away from the forms would mean resigning the role that gives their lives their shape; they cannot advocate relaxing what they administer without unmaking themselves.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, liturgical_council, agenda_setter,
    institutional, generational, identity_locked, continental).

% Lived the catastrophe first-hand. The rites carry their dead — names recited in fixed formula, anniversaries kept on the inherited calendar — and they experience deviation as a second death. They fund and defend the forms, attend faithfully, and police drift among the young. At the same time their living testimony is subordinated to liturgy: grief must take the received shape, and proposals to alter mourning practice read, from inside, as abandoning the dead. Leaving the practice would mean losing the only community in which their loss is legible.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, survivor_generation, beneficiary,
    organized, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, survivor_generation, payer).

% Inherited the forms without the memories that made them. Compliance costs real resources: years of ancestral-language schooling, observance that constrains careers, endogamy pressure that narrows partnership, commemorative obligations that anchor families to scattered sites. They also receive what the forms provide — belonging, an answer to who they are, a community that shows up in crisis. Proposing change marks them disloyal; leaving means forfeiting the community entirely, so most comply and adapt only privately at the margins.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, younger_membership, payer,
    moderate, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_transmission__symbol_continuity_reading, younger_membership, beneficiary).

% Push for adapted forms: vernacular mourning rites, a shortened commemorative calendar, translated liturgy, flexibility for members in mixed partnerships. Every channel of influence runs through bodies the council controls, and past reform attempts ended in credential revocation, bans from officiating, and social marking as assimilationists. They stay inside because exit would hand the community entirely to the fidelity party; formally, they are heard nowhere.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, reform_advocates, excluded,
    moderate, biographical, trapped, continental).

% Comparative researchers of post-catastrophe communities. They document cases where communities without portable ritual carriers dissolved within two generations and cases where communities that adapted their forms retained membership while changing practice. They publish, advise, and hold no office in the community; nothing in their standing depends on which account of the forms prevails.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__symbol_continuity_reading, memory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__symbol_continuity_reading, liturgical_council).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__symbol_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-catastrophe cohesion problem: a scattered, decimated population needs a portable, low-infrastructure carrier of common identity and a synchronized calendar for collective mourning, mutual-aid mobilization, and membership recognition across distance and generations. Fixed symbolic form does this without requiring territory or rebuilt institutions.
% TRANSFER_FUNCTION: Moves time, labor, language-learning effort, and discretionary resources from ordinary members — disproportionately the young — into the production and maintenance of fixed ritual form; moves deference, authority, and livelihood security to the officiants who administer it; moves recognition and belonging back to members in proportion to compliance.
% ABSENT_VOICES: Reform advocates sit outside every decision body and are sanctioned when they speak. The young are present demographically but structurally muted, since fidelity is defined by those who received the forms at first hand. Most fundamentally, the dead are invoked as the arrangement's authors and intended beneficiaries but cannot consent to what is preserved in their name — the forms claim to speak for them.
% DISAPPEARANCE_RATIONALE: If the fidelity regime vanished overnight, the community would not simply continue minus overhead: membership recognition, the mourning calendar, marriage and education patterns, and the council's authority all hang on the forms. Some members would rebuild adapted equivalents within a generation; others would drift into host societies, and the community as a bounded body would likely fragment or dissolve — either way, the world rearranges.
% FOUNDING_PROBLEM: After catastrophic destruction — mass death, displacement, loss of territory and institutions — the community faced dissolution: nothing portable remained that could carry identity, coordinate mourning, and bind scattered survivors into one continuing body.
% FOUNDING_PROBLEM_CORROBORATION: Outside the benefiting parties, comparative historians and trauma sociologists corroborate that the founding problem was real: documented cases of comparably devastated communities without portable ritual carriers failed to reconstitute. Whether it remains live is disputed along the same external seam — liturgical authorities cite ongoing assimilation attrition, while third-generation membership studies and diaspora sociologists find the acute cohesion problem transformed into an ordinary pluralism problem. No party outside the beneficiary set attests that the original acute-phase problem persists unchanged.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__symbol_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__symbol_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__symbol_continuity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(catastrophe_memory_transmission__symbol_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__symbol_continuity_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_transmission__symbol_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_transmission__symbol_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the fidelity regime transfers real resources and forecloses real choices, but roughly half of what it consumes purchases goods members would partly secure anyway (cohesion, mourning, membership recognition); the remainder is asymmetry — adaptation options closed for the young, authority rents for the council. Suppression 0.60: early adherence was voluntary (shared trauma), but each generational drift has required renewed enforcement — sanction of reformers, policing of vernacular drift, marriage and education pressure. Theater_ratio 0.26: the rites function (grief is processed, identity transmitted), but a growing minority of compliance is performative — attendance without interiority — concentrated in the youngest cohort. Accessibility_collapse 0.45: alternatives exist (secular commemoration, host-society belonging, private adapted practice) but each carries real social cost, so alternatives are degraded rather than eliminated. Resistance 0.50: recurring reform movements, episodic and contained. CYCLICAL PATTERN: the suppression series oscillates around a rising ratchet — reform crisis (crackdown, T≈20 and T≈50) → accommodation (relaxation) → renewed drift (accumulation) → next crisis; each cycle settles at a higher enforcement baseline than the last, and that ratchet is itself the extraction mechanism (intermittent reinforcement: conformity repurchased repeatedly at escalating price). Base_properties were measured at interval end (T=80), on the rising side of the latest cycle. All three metric series share one time grid (0–80 by decades) so no scalar substitution contaminates earlier points. COALITION NOTE: younger_membership and reform_advocates overlap demographically; a sustained coalition is the main structural threat to the ratchet, which is precisely why enforcement targets reformer credentialing first. BOLTZMANN NOTE: identity_coordination is declared because the dominant function is membership-boundary and identity maintenance; the type's complexity offset accommodates genuine boundary-maintenance complexity but must not excuse the observed concentration — enforcement weight falls on moderate-power members across continental scope, which is flagged for review rather than passed. Suppression is authored as a raw structural property and is NOT scaled by scope; only extractiveness is engine-scaled (directionality × scope — the community's continental scatter makes verification harder, modestly amplifying effective extraction for target seats).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the council seat the arrangement is the coordination it personally maintains — fidelity looks like stewardship, and enforcement like care for the dead. From the younger_membership seat the same structure is a tax levied in the currency of loyalty: belonging priced in foreclosed adaptation. Survivor_generation straddles the gap — beneficiaries who are also bound. Reform_advocates see pure foreclosure; memory_scholars see both faces in comparative data. The engine computes these per-seat classifications from power/exit/role data; this commentary explains why they diverge, not which is right. Identity-lock mechanics differ by seat: the council's lock is institutional (it has become its function — dismantle the forms and there is no council), the survivors' is relational (self-concept constituted through mourning the dead in the received form), the young show partial ideological lock (betrayal-framing internalized). If the survivors' relational frame broke — for instance, an accepted alternative memorial form — enforcement demand would collapse faster than any rule change, and the arrangement would drift toward a transitional, sunset-able support.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: liturgical_council and survivor_generation sit near the beneficiary pole (low d); younger_membership and reform_advocates near the target pole (high d). One override: survivor_generation is declared a beneficiary and the derivation would read them as near-pure beneficiaries (~0.15–0.2), but their structural position is genuinely dual — identity_locked exit, living testimony subordinated to liturgical form, grief channeled into fixed expression they did not choose — so d is overridden to 0.35, reflecting meaningful target-side exposure. younger_membership's secondary beneficiary role (belonging, meaning, crisis solidarity) damps their derived d below reform_advocates', who receive little and are sanctioned for proposing change. Scope: the community's continental scatter raises verification costs, which the engine applies as a modest upward modifier to effective extraction for target seats; the scholars' analytical seat sits outside the extraction circuit entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   Mislabeling risks run both ways. Calling this a snare erases the corroborated coordination achievement — communities without portable ritual carriers demonstrably dissolved — and would treat mourning itself as extraction. Calling it a rope ignores the measured asymmetry: the young pay in foreclosed adaptation what the old collect in authority and continuity, enforced by a ratcheting apparatus. Tangled_rope holds both facts. On mandatrophy: the founding problem (acute post-catastrophe dissolution risk) has not plainly died — assimilation attrition is real — but it has plainly transformed (third-generation members face ordinary pluralism, not annihilation); hence founding_problem_status 'contested' rather than a clean 'dead,' and no mandatrophy_resolved declaration. The R5 mismatch consumer should read the contested status against the world_rearranges verdict: the arrangement is load-bearing today, but for a partially different problem than the one that founded it — the classic pre-condition of quiet capture, watched here rather than declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (symbol_continuity) of the kernel catastrophe_memory_transmission; what structurally changes if a sibling reading is adopted instead?',
    'Cross-reading compilation: generate the operational_competence and hybrid_embedded sibling stories, compute per-seat classifications over the same structural substrate, and diff the type and effective-extraction outputs.',
    'Under the operational or hybrid reading, much of the measured fidelity cost re-reads as competence tuition or embedded-knowledge maintenance, lowering ε and potentially shifting the computed type from tangled_rope toward rope; under this reading the foreclosure stands as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification of a shared kernel; committer structure routed here rather than invented into standard fields.').

omega_variable(
    mechanism_attribution_dispute,
    'Where exactly do the readings disagree — on what the forms transmit (mechanism attribution) or on the evaluative status of form-preservation itself (intrinsic versus instrumental good)?',
    'Matched-cohort testing: compare form-trained and non-form-trained community members on concrete coordination and threat-assessment tasks; if training effects appear independent of identity commitment, the mechanism dispute resolves toward the sibling readings.',
    'If the forms demonstrably transmit operational content, part of this reading''s ε is overstated (cost of tuition rather than foreclosure), softening the tangled_rope verdict; if not, the intrinsic-good claim carries the arrangement alone and the extraction reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mechanism_attribution_dispute, conceptual, 'Location of the inter-reading disagreement: mechanism attribution versus evaluative status of the forms.').

omega_variable(
    fidelity_suppression_internalization,
    'Is the measured suppression (0.60) primarily structural (sanctions, credentialing denial, marriage pressure) or internalized (betrayal-framing and guilt toward the dead that persist without enforcement)?',
    'Post-exit trajectory study: interview members who left the community; if betrayal-guilt and fidelity compulsion persist years after exit with no structural enforcement in reach, the internalized share is large.',
    'If largely internalized, effective suppression exceeds the structural measure — leavers carry the arrangement with them, per-seat suppression for identity_locked seats rises, and enforcement decay would not relax the constraint as fast as the suppression series alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_suppression_internalization, empirical, 'Structural versus internalized split of fidelity enforcement.').

omega_variable(
    foregone_adaptation_magnitude,
    'How much adaptive capacity does fidelity actually foreclose — time, capital, linguistic isolation, endogamy pressure, commemorative-site constraints — as distinct from costs members would voluntarily bear?',
    'Counterfactual comparison with demographically matched post-catastrophe communities under relaxed fidelity regimes, controlling for host-society conditions.',
    'A large foregone-adaptation share raises payer-seat effective extraction and, if the burden stays concentrated on the young while gains concentrate in the council, pushes the computed classification toward snare; a small share supports the coordination-first reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(foregone_adaptation_magnitude, empirical, 'Magnitude of adaptive capacity sacrificed to form-preservation.').

omega_variable(
    gain_capture_vs_diffusion,
    'Does the liturgical_council capture the arrangement''s net gains (deference, livelihood, veto over adaptation), or do gains diffuse as identity continuity across all members?',
    'Resource-flow audit of communal institutions: trace dues, endowment income, and officiant compensation against enforcement activity; compare council welfare trajectories with member-base trajectories across the interval.',
    'Concentrated capture confirms the receipt-surface verdict and sharpens the extraction asymmetry; genuinely diffuse gains would reposition the arrangement closer to a pure coordination good carrying legacy enforcement overhead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gain_capture_vs_diffusion, empirical, 'Whether gains concentrate in the administering seat or diffuse across the membership.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__symbol_continuity_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catmem_symcon_tr_t0, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(catmem_symcon_tr_t0, observed).
narrative_ontology:measurement(catmem_symcon_tr_t10, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement_basis(catmem_symcon_tr_t10, observed).
narrative_ontology:measurement(catmem_symcon_tr_t20, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(catmem_symcon_tr_t20, observed).
narrative_ontology:measurement(catmem_symcon_tr_t30, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(catmem_symcon_tr_t30, observed).
narrative_ontology:measurement(catmem_symcon_tr_t40, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement_basis(catmem_symcon_tr_t40, observed).
narrative_ontology:measurement(catmem_symcon_tr_t50, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(catmem_symcon_tr_t50, observed).
narrative_ontology:measurement(catmem_symcon_tr_t60, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 60, 0.23).
narrative_ontology:measurement_basis(catmem_symcon_tr_t60, observed).
narrative_ontology:measurement(catmem_symcon_tr_t70, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 70, 0.25).
narrative_ontology:measurement_basis(catmem_symcon_tr_t70, observed).
narrative_ontology:measurement(catmem_symcon_tr_t80, catastrophe_memory_transmission__symbol_continuity_reading, theater_ratio, 80, 0.26).
narrative_ontology:measurement_basis(catmem_symcon_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(catmem_symcon_be_t0, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(catmem_symcon_be_t0, observed).
narrative_ontology:measurement(catmem_symcon_be_t10, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 10, 0.37).
narrative_ontology:measurement_basis(catmem_symcon_be_t10, observed).
narrative_ontology:measurement(catmem_symcon_be_t20, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(catmem_symcon_be_t20, observed).
narrative_ontology:measurement(catmem_symcon_be_t30, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 30, 0.43).
narrative_ontology:measurement_basis(catmem_symcon_be_t30, observed).
narrative_ontology:measurement(catmem_symcon_be_t40, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 40, 0.5).
narrative_ontology:measurement_basis(catmem_symcon_be_t40, observed).
narrative_ontology:measurement(catmem_symcon_be_t50, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 50, 0.47).
narrative_ontology:measurement_basis(catmem_symcon_be_t50, observed).
narrative_ontology:measurement(catmem_symcon_be_t60, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 60, 0.53).
narrative_ontology:measurement_basis(catmem_symcon_be_t60, observed).
narrative_ontology:measurement(catmem_symcon_be_t70, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 70, 0.56).
narrative_ontology:measurement_basis(catmem_symcon_be_t70, observed).
narrative_ontology:measurement(catmem_symcon_be_t80, catastrophe_memory_transmission__symbol_continuity_reading, base_extractiveness, 80, 0.58).
narrative_ontology:measurement_basis(catmem_symcon_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(catmem_symcon_su_t0, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(catmem_symcon_su_t0, observed).
narrative_ontology:measurement(catmem_symcon_su_t10, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 10, 0.33).
narrative_ontology:measurement_basis(catmem_symcon_su_t10, observed).
narrative_ontology:measurement(catmem_symcon_su_t20, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 20, 0.47).
narrative_ontology:measurement_basis(catmem_symcon_su_t20, observed).
narrative_ontology:measurement(catmem_symcon_su_t30, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 30, 0.41).
narrative_ontology:measurement_basis(catmem_symcon_su_t30, observed).
narrative_ontology:measurement(catmem_symcon_su_t40, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 40, 0.5).
narrative_ontology:measurement_basis(catmem_symcon_su_t40, observed).
narrative_ontology:measurement(catmem_symcon_su_t50, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 50, 0.44).
narrative_ontology:measurement_basis(catmem_symcon_su_t50, observed).
narrative_ontology:measurement(catmem_symcon_su_t60, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement_basis(catmem_symcon_su_t60, observed).
narrative_ontology:measurement(catmem_symcon_su_t70, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 70, 0.57).
narrative_ontology:measurement_basis(catmem_symcon_su_t70, observed).
narrative_ontology:measurement(catmem_symcon_su_t80, catastrophe_memory_transmission__symbol_continuity_reading, suppression_requirement, 80, 0.6).
narrative_ontology:measurement_basis(catmem_symcon_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__symbol_continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__operational_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_transmission__symbol_continuity_reading, catastrophe_memory_transmission__hybrid_embedded_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'ritual preserves the community after catastrophe' decomposes per the ε-invariance principle into three readings of one kernel, each a separate constraint with its own ε and beneficiary/victim emphasis. This (symbol-continuity) story authors ε≈0.58 for the fidelity regime read as identity-good-plus-adaptation-foreclosure; the operational_competence sibling authors the same forms as competence carriers (lower ε — much of the fidelity cost re-reads as tuition); the hybrid sibling relocates the locus (form and competence inseparable). Edges run from this, the institutionally entrenched reading, to the siblings because fidelity enforcement sets the operating environment within which competence-claims can be tested. The decomposition is documented in both directions; no story averages across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_transmission__symbol_continuity_reading, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
