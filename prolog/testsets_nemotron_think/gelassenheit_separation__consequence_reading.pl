% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Amish Technology Evaluation by Community Consequences (Consequence Reading)
 *   domain: religious/technology_governance/commitment_systems
 *
 * SUMMARY:
 *   The consequence_reading of gelassenheit_separation is the Amish Ordnung's
 *   most flexible interpretive strand: technology is evaluated not by its
 *   appearance (artifact_reading) or its systemic entanglement
 *   (principle_reading) but by its observable consequences for the three
 *   pillars of community life — visiting, mutual aid, and geographic
 *   rootedness. This reading permits telephones in barns (business/emergency
 *   use preserves rootedness) but forbids them in homes (erodes visiting); it
 *   permits tractors for stationary belt power (threshing, sawing) but not
 *   for field cultivation (replaces neighborly labor exchange). The
 *   constraint is a living case-law tradition, not a fixed code.
 *   Extractiveness is low (0.28) because most members experience the rules as
 *   coordination, not extraction — but constrained_members bear real costs,
 *   and enforcement (shunning) is active and severe, making this a
 *   tangled_rope, not a pure rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.28).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.45).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, tangled_rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Amish Technology Evaluation by Community Consequences (Consequence Reading)").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/technology_governance/commitment_systems").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '6fb0a712-6b44-44c7-a20a-808fd315eb71').
narrative_ontology:cs_kernel_codification('6fb0a712-6b44-44c7-a20a-808fd315eb71', distributed).
narrative_ontology:cs_authority_grounding('6fb0a712-6b44-44c7-a20a-808fd315eb71', lineage).
narrative_ontology:cs_interpretation_layer_present('6fb0a712-6b44-44c7-a20a-808fd315eb71').
narrative_ontology:cs_reading_relation('6fb0a712-6b44-44c7-a20a-808fd315eb71', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('6fb0a712-6b44-44c7-a20a-808fd315eb71', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('6fb0a712-6b44-44c7-a20a-808fd315eb71', foundational, technology_evaluated_by_community_consequences).
narrative_ontology:cs_axiom_status(technology_evaluated_by_community_consequences, holdable).
narrative_ontology:cs_axiom_grounding('6fb0a712-6b44-44c7-a20a-808fd315eb71', technology_evaluated_by_community_consequences, conventional).
narrative_ontology:cs_axiom('6fb0a712-6b44-44c7-a20a-808fd315eb71', foundational, visiting_mutual_aid_rootedness_preserved).
narrative_ontology:cs_axiom_status(visiting_mutual_aid_rootedness_preserved, holdable).
narrative_ontology:cs_axiom_grounding('6fb0a712-6b44-44c7-a20a-808fd315eb71', visiting_mutual_aid_rootedness_preserved, conventional).
narrative_ontology:cs_reference_frame('6fb0a712-6b44-44c7-a20a-808fd315eb71', community_practice_preservation).
narrative_ontology:cs_drift_state('6fb0a712-6b44-44c7-a20a-808fd315eb71', contemporary_technology_pressure, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6fb0a712-6b44-44c7-a20a-808fd315eb71', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, compliant_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, bishops_ministers).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, constrained_members).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, gelassenheit_as_community_preservation).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, technology_subordinate_to_social_goods).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply the Ordnung's technology rules case by case, evaluating each innovation by its observed effects on visiting patterns, mutual aid networks, and geographic rootedness. They authorize telephones in barns for business/emergency use but forbid them in homes where they would disrupt face-to-face visiting. They authorize tractors for belt-powered stationary work (threshing, sawing) but not for field cultivation that would reduce dependence on neighborly labor exchange. Their authority derives from ordination lineage and communal recognition; they bear the burden of discernment but also the legitimacy that comes with it.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, bishops_ministers, agenda_setter,
    institutional, generational, analytical, regional).

% Accept the technology rules as the price of community membership. They experience the coordination benefits: dense visiting networks, reliable mutual aid (barn raisings, harvest crews, illness support), and children raised in a geographically rooted lifeworld. The rules are not experienced as extraction but as the conditions that make their form of life possible. Exit would mean shunning (Meidung) — loss of family, community, and identity — so exit_options is identity_locked, not merely constrained.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, compliant_members, beneficiary,
    organized, biographical, identity_locked, local).

% Want technologies the consequence_reading forbids — a telephone in the house for aging parents, a tractor for field work to spare aging bodies, internet access for children's education. They bear the cost of the rule: foregone convenience, physical strain, educational disadvantage. They argue the rules are inconsistently applied (why barn phones but not house phones? why belt power but not field power?). Their exit is also identity_locked — leaving means shunning — so they stay and negotiate, petition, or quietly circumvent.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, constrained_members, payer,
    moderate, biographical, identity_locked, local).

% In the liminal period before baptism (typically ages 16-22), they experience English technology freely — smartphones, cars, social media — but have no voice in the Ordnung that will govern them if they join. They would object to restrictions they have not yet consented to, but the community's structure excludes them from discernment until baptism. Their exit is constrained: return to the community on its terms, or leave permanently and face shunning.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, youth_in_rumspringa, excluded,
    powerless, immediate, constrained, local).

% Non-Amish farmers and townspeople who interact economically with the community (buying produce, hiring crews, selling equipment). They observe the technology rules as a market constraint — e.g., they cannot sell certain equipment to Amish customers, or must adapt equipment (steel wheels, belt-power adapters). They have no standing in the community's discernment but are affected by its boundaries.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, english_neighbors, observer,
    moderate, immediate, mobile, regional).

% Academics, journalists, and policy analysts who study Amish technology governance as a case of intentional technological choice. They document the consequence_reading's logic but have no stake in its enforcement. Their analysis feeds back into public discourse about technology ethics, occasionally influencing how the community explains itself to outsiders.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, scholars_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the dense, face-to-face social fabric that makes mutual aid and geographic rootedness viable: visiting (daily/weekly household visits) sustains kinship and fellowship; mutual aid (labor exchange, crisis response) replaces insurance and state services; geographic rootedness (staying on the land, near kin) prevents the dispersion that dissolves community. The technology rules are the boundary-maintenance mechanism that keeps these practices from eroding.
% TRANSFER_FUNCTION: Moves autonomy over technology adoption from the individual/household to the communal discernment process. The constrained_member gives up specific technologies (house phones, field tractors, internet); the community receives the preservation of visiting/mutual-aid/rootedness. The bishops_ministers receive interpretive authority and the legitimacy that comes with maintaining the boundary.
% ABSENT_VOICES: Youth in rumspringa (excluded from discernment until baptism); former members who left (shunned, cannot return to advocate); constrained_members who silently comply but would dissent if exit were less costly; English neighbors who adapt their businesses to Amish rules but have no say in them.
% DISAPPEARANCE_RATIONALE: If the consequence_reading's technology rules vanished overnight, visiting patterns would collapse within a generation (phones/screens displace face-to-face interaction), mutual aid would wither (tractors replace labor exchange, insurance replaces communal risk-pooling), and geographic rootedness would dissolve (cars/internet enable dispersion). The community would either reorganize around a new boundary-maintenance mechanism or cease to be a distinct lifeworld.
% FOUNDING_PROBLEM: Early Anabaptist communities in Europe and America faced dissolution through persecution-driven dispersion, assimilation into surrounding cultures, and the lure of modern conveniences that made communal self-sufficiency seem unnecessary. The Ordnung emerged as a collective answer: how to remain a distinct people in the world but not of it, without freezing all technology at a single historical moment.
% FOUNDING_PROBLEM_CORROBORATION: The community's own historians and bishops attest the problem is live — contemporary technology (smartphones, AI, genetic medicine) poses new boundary questions. Scholars outside the community (Kraybill, Hostetler, Nolt) corroborate that the founding problem — maintaining distinctiveness amid technological change — remains the central tension. No non-beneficiary source claims the problem is dead; the contested status reflects disagreement about whether the consequence_reading's flexible case-by-case method still serves the founding problem or has become a cover for incremental assimilation.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.28, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28) reflects that the constraint's primary operation is coordination — the rules are finely tuned to preserve specific practices, not to extract resources. Suppression (0.45) is moderate: shunning is a real coercive threat, but most compliance is voluntary/internalized. Theater_ratio (0.30) has risen as the case law accumulates — some distinctions (barn vs. house phone) look performative to constrained_members and scholars. Accessibility_collapse (0.60): alternatives exist (leave, petition, circumvent) but are costly. Resistance (0.40): constrained_members petition for rule changes; some districts have split over technology disputes.
 *
 * PERSPECTIVAL GAP:
 *   From the bishops_ministers' seat, the constraint is genuine coordination — they do the discernment work and see the community hold together. From compliant_members' seat, it is beneficial coordination — they get the social goods. From constrained_members' seat, it is extraction with identity-locked exit — they pay costs they cannot escape. From youth_in_rumspringa's seat, it is an unchosen imposition. The engine computes these divergences from the structural data; the claimed_type (tangled_rope) reflects the analyzer's judgment that the extraction/coordination hybrid is structural, not perspectival.
 *
 * DIRECTIONALITY LOGIC:
 *   Bishops_ministers are agenda_setters with analytical exit (they interpret, they don't merely comply) — d near beneficiary end. Compliant_members are beneficiaries with identity_locked exit — d near symmetric (they benefit and bear compliance costs willingly). Constrained_members are payers with identity_locked exit — d near target end (they bear costs they did not choose). Youth_in_rumspringa are excluded — not yet subject to the constraint but structurally positioned to become payers. English_neighbors and scholars are observers with mobile/analytical exit — d near zero. The identity_locked exit for both compliant and constrained members is the key amplifier: the same rule that is coordination for one is extraction for the other because neither can exit without shunning.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining distinctiveness amid technological change) is live — new technologies (smartphones, gene editing, AI) create novel boundary questions. The consequence_reading's case-by-case method is its adaptive mechanism. Mandatrophy would occur if the method became a rubber stamp for assimilation (always permitting) or fossilized into artifact-based bans (always forbidding). Currently neither: the method still generates live discernment (e.g., recent debates over solar panels, electric bikes, telemedicine). The founding_problem_status = live and disappearance_verdict = world_rearranges together indicate no mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_boundary,
    'Is the consequence_reading a distinct constraint from the artifact_reading and principle_reading, or are they interpretive variants of a single constraint?',
    'Compare epsilon values and victim sets across readings. If epsilon differs substantially (e.g., artifact_reading has higher extractiveness because it bans more technologies categorically) and victim sets differ (artifact_reading constrains more members), they are distinct constraints per ε-invariance.',
    'If distinct, each reading gets its own classification and the kernel is a family. If variants, they are one constraint with observer-dependent classification — which the framework rejects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_boundary, conceptual, 'Whether the three readings instantiate one constraint or three.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.45) structural (shunning as external sanction) or internalized (members believe the rules are right)?',
    'Post-exit suppression trajectory: if former members report persistent guilt/anxiety about technology use after leaving, internalized component is significant. If suppression drops to near zero post-exit, it is primarily structural.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. This would raise effective extraction for identity_locked payers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression in a high-identity-locked community.').

omega_variable(
    coordination_extraction_boundary,
    'Where does the consequence_reading''s coordination function end and extraction begin? Is the barn-vs-house phone distinction a genuine coordination boundary or an arbitrary line that extracts from constrained_members?',
    'Natural experiment: districts that have permitted house phones — did visiting patterns actually collapse? If not, the distinction is extractive. If visiting did collapse, it is coordinative.',
    'If the distinction is extractive, the constraint''s epsilon is understated and claimed_type should shift toward snare. If coordinative, tangled_rope stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether fine-grained technology rules serve coordination or mask extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 74).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(gela_tr_t18, gelassenheit_separation__consequence_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement(gela_tr_t36, gelassenheit_separation__consequence_reading, theater_ratio, 36, 0.22).
narrative_ontology:measurement(gela_tr_t54, gelassenheit_separation__consequence_reading, theater_ratio, 54, 0.27).
narrative_ontology:measurement(gela_tr_t74, gelassenheit_separation__consequence_reading, theater_ratio, 74, 0.3).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(gela_be_t18, gelassenheit_separation__consequence_reading, base_extractiveness, 18, 0.18).
narrative_ontology:measurement(gela_be_t36, gelassenheit_separation__consequence_reading, base_extractiveness, 36, 0.22).
narrative_ontology:measurement(gela_be_t54, gelassenheit_separation__consequence_reading, base_extractiveness, 54, 0.25).
narrative_ontology:measurement(gela_be_t74, gelassenheit_separation__consequence_reading, base_extractiveness, 74, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(gela_su_t18, gelassenheit_separation__consequence_reading, suppression_requirement, 18, 0.35).
narrative_ontology:measurement(gela_su_t36, gelassenheit_separation__consequence_reading, suppression_requirement, 36, 0.4).
narrative_ontology:measurement(gela_su_t54, gelassenheit_separation__consequence_reading, suppression_requirement, 54, 0.43).
narrative_ontology:measurement(gela_su_t74, gelassenheit_separation__consequence_reading, suppression_requirement, 74, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.08).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% The gelassenheit_separation kernel decomposes into three constraint stories (artifact_reading, consequence_reading, principle_reading) with different epsilon values, victim sets, and coordination logics. The consequence_reading has the lowest epsilon (0.28) and most contextual rules; artifact_reading has higher epsilon (categorical bans); principle_reading has moderate epsilon but different victim set (those entangled in worldly systems). They form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, institutional, 0.15).
constraint_indexing:directionality_override(gelassenheit_separation__consequence_reading, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
