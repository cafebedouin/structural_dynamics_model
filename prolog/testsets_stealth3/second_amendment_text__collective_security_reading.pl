% ============================================================================
% CONSTRAINT STORY: second_amendment_text__collective_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_second_amendment_text__collective_security_reading, []).

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
 *   constraint_id: second_amendment_text__collective_security_reading
 *   human_readable: Second Amendment — Collective-Security Reading (Militia-Conditioned Right)
 *   domain: constitutional law/political theory/firearms policy
 *
 * SUMMARY:
 *   The constraint instantiated here is the collective-security reading of
 *   the Second Amendment: the militia clause conditions the right to keep and
 *   bear arms on organized civic defense, and the state may regulate arms to
 *   serve collective security. Its ε referent is the standing
 *   militia-conditioned arrangement — the licensing, permit, and
 *   category-restriction apparatus operating under collective-security
 *   justification — as this reading itself assesses it, never a rival
 *   arrangement. The reading sees genuine coordination (collective security
 *   without a standing army; armed force subordinated to civil authority) and
 *   real asymmetric cost (gun owners bear compliance burdens; the apparatus
 *   collects fees, budgets, and discretion). The measurement interval covers
 *   the modern regulatory era (1934–2024), over which regulation expanded
 *   well beyond militia-related purposes while the militia institutions
 *   naming the condition atrophied, and federal doctrine repudiated the
 *   reading in 2008 — leaving state-level persistence. This story is one
 *   member of a three-file constraint family decomposing the
 *   second_amendment_text kernel (see network.dual_formulation_note); the
 *   sibling readings are separate constraints with their own ε and are not
 *   averaged here. Claim and metrics are authored independently: the claimed
 *   type states what the structure shows, and the metrics describe the
 *   arrangement's actual operation under this reading's lights.
 *
 * KEY AGENTS:
 *   - state_regulatory_apparatus: agenda-setter and primary beneficiary seat (institutional/arbitrage) — writes the conditions, collects the fees and enforcement budgets
 *   - individual_gun_owners: primary target seat (moderate/constrained) — bears licensing and compliance costs under a condition it does not administer
 *   - national_guard_system: institutional beneficiary (institutional/constrained) — the organized-militia referent the condition names
 *   - general_public: diffuse beneficiary with payer overlay (organized/constrained) — receives collective security, funds the apparatus, sits inside the regulatory perimeter
 *   - unorganized_militia_members: secondary target seat (moderate/trapped) — involuntary statutory membership, obligation without structure
 *   - private_militia_organizers: excluded seat — defined out of the condition by its 'organized' requirement
 *   - federal_courts: interpretive authority (institutional/analytical) — adjudicates the condition's scope; repudiated the reading federally in 2008
 *   - constitutional_scholars: analytical observer — supplies the doctrinal history all seats draw on
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, 0.52).
domain_priors:suppression_score(second_amendment_text__collective_security_reading, 0.55).
domain_priors:theater_ratio(second_amendment_text__collective_security_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(second_amendment_text__collective_security_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(second_amendment_text__collective_security_reading, tangled_rope).
narrative_ontology:human_readable(second_amendment_text__collective_security_reading, "Second Amendment — Collective-Security Reading (Militia-Conditioned Right)").
narrative_ontology:topic_domain(second_amendment_text__collective_security_reading, "constitutional law/political theory/firearms policy").

domain_priors:requires_active_enforcement(second_amendment_text__collective_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(second_amendment_text__collective_security_reading, 'e5fe6ec8-cf27-4496-b51c-7925b32ba965').
narrative_ontology:cs_kernel_codification('e5fe6ec8-cf27-4496-b51c-7925b32ba965', fixed_text).
narrative_ontology:cs_authority_grounding('e5fe6ec8-cf27-4496-b51c-7925b32ba965', lineage).
narrative_ontology:cs_interpretation_layer_present('e5fe6ec8-cf27-4496-b51c-7925b32ba965').
narrative_ontology:cs_reading_relation('e5fe6ec8-cf27-4496-b51c-7925b32ba965', second_amendment_text__individual_right_reading, forecloses).
narrative_ontology:cs_reading_relation('e5fe6ec8-cf27-4496-b51c-7925b32ba965', second_amendment_text__originalist_civic_virtue_reading, forecloses).
narrative_ontology:cs_axiom('e5fe6ec8-cf27-4496-b51c-7925b32ba965', foundational, militia_clause_conditions_operative_clause).
narrative_ontology:cs_axiom_status(militia_clause_conditions_operative_clause, holdable).
narrative_ontology:cs_axiom_grounding('e5fe6ec8-cf27-4496-b51c-7925b32ba965', militia_clause_conditions_operative_clause, conventional).
narrative_ontology:cs_axiom('e5fe6ec8-cf27-4496-b51c-7925b32ba965', secondary, militia_relationship_defines_protected_arms).
narrative_ontology:cs_axiom_status(militia_relationship_defines_protected_arms, holdable).
narrative_ontology:cs_axiom_grounding('e5fe6ec8-cf27-4496-b51c-7925b32ba965', militia_relationship_defines_protected_arms, instrumental).
narrative_ontology:cs_reference_frame('e5fe6ec8-cf27-4496-b51c-7925b32ba965', militia_conditioned_civic_defense_settlement).
narrative_ontology:cs_drift_state('e5fe6ec8-cf27-4496-b51c-7925b32ba965', post_heller_post_bruen_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e5fe6ec8-cf27-4496-b51c-7925b32ba965', '').
narrative_ontology:cs_kernel_id(second_amendment_text__collective_security_reading, second_amendment_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, national_guard_system).
narrative_ontology:constraint_beneficiary(second_amendment_text__collective_security_reading, general_public).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, individual_gun_owners).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, unorganized_militia_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(second_amendment_text__collective_security_reading, general_public).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, miller_militia_relationship_test).
narrative_ontology:constraint_vindicates(second_amendment_text__collective_security_reading, collective_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% State legislatures, police agencies, and licensing bureaus set the terms on which arms access is conditioned: permit requirements, fees, waiting periods, category restrictions. They collect licensing revenue and justify enforcement budgets against the collective-security mandate. Their exit from the arrangement is redefinition — they write the conditions and can rewrite them; what they cannot do is abandon the condition without conceding the regulatory authority it carries.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, state_regulatory_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, state_regulatory_apparatus, beneficiary).

% The institutional descendant of the founding-era militia. It receives the constitutional anchoring the militia clause provides — its status as the organized militia is what the condition's 'organized civic defense' points to. It does not administer licensing, but its institutional continuity, federal funding, and civic role depend on the militia framework remaining the constitutional referent for arms-bearing.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, national_guard_system, beneficiary,
    institutional, generational, constrained, national).

% Receives the collective-security provision the condition exists to secure, and funds the apparatus that administers it. Every member is also inside the regulatory perimeter — background checks and category restrictions apply to all purchasers regardless of militia relevance. Exit from the framework means relocating among states; the federal constitutional floor travels with them.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, general_public, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(second_amendment_text__collective_security_reading, general_public, payer).

% Bear the condition's costs directly: permit fees, waiting periods, registration requirements, and exclusion from restricted categories. The scope of what they may keep and bear is set by a condition they do not administer. Relocating to a permissive state is available but costly — arms are durable goods, and for many owners the practice carries personal and family investment; the federal floor follows them across state lines.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, individual_gun_owners, payer,
    moderate, biographical, constrained, national).

% The statutory residue of the constitutional militia: able-bodied citizens liable to militia duty who are not enrolled in any organized force. Membership attaches by age and status, not choice, so it cannot be resigned. They carry the condition's civic obligation without the organized structure the condition presupposes; the category they belong to persists in statute while the institution it names has largely atrophied.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, unorganized_militia_members, payer,
    moderate, biographical, trapped, national).

% Citizens who would organize armed civic defense outside state command structures. The condition reserves 'organized' civic defense to state-controlled institutions, so these groups cannot gain standing within the constitutional framework no matter how they structure themselves. They appear in the conversation only as objects of legislation restricting private paramilitary organization, not as participants in defining the condition.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, private_militia_organizers, excluded,
    moderate, biographical, trapped, national).

% Adjudicate the condition's scope: what counts as a militia relationship, which regulations serve collective security, where the condition's limits sit. Their doctrines defined and redefined the arrangement's operation across the interval, and their 2008 repudiation of the militia-relationship test reshaped where it can still govern. They collect no fees and bear no compliance costs; their stake is interpretive authority.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, federal_courts, observer,
    institutional, generational, analytical, national).

% Produce the historical and doctrinal analyses the courts draw on — ratification debates, militia statutes, founding-era practice. The profession spans generations and holds no budget or compliance stake; its product shapes which understandings of the condition are available to decision-makers.
narrative_ontology:constraint_stakeholder(second_amendment_text__collective_security_reading, constitutional_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(second_amendment_text__collective_security_reading, state_regulatory_apparatus).
narrative_ontology:fixing_cost_class(second_amendment_text__collective_security_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Channels the citizenry's arms-bearing capacity into state-organized civic defense: the militia system addresses the founding-era collective-security problem — defense without a standing army — while keeping armed force subordinate to civil authority, and standardizes the terms on which arms access is conditioned on that organization.
% TRANSFER_FUNCTION: Moves regulatory discretion over arms possession from individuals to state legislative and administrative bodies; moves licensing revenue and enforcement funding from the regulated population to the apparatus; moves security provision from private self-help to collectively organized defense.
% ABSENT_VOICES: Citizens who hold their arms-keeping as a personal capacity rather than a civic function would object that the condition subordinates their claim to a structure they did not join; they sit outside the legislative and judicial forums where the condition is administered, entering only as litigants and voters. Private militia organizers (a named excluded seat) are defined out of the conversation entirely by the condition's 'organized' requirement.
% DISAPPEARANCE_RATIONALE: If the conditioned-right arrangement vanished overnight — the right became unconditional and the condition's regulatory authority lapsed — licensing regimes would lose their constitutional anchor, state police-power approaches to arms would reorganize around general public-safety doctrines, the National Guard's constitutional anchoring would need re-founding, and the apparatus would lose its fee and enforcement base. The regulatory perimeter around arms would not persist on inertia alone.
% FOUNDING_PROBLEM: The founding-era problem: securing collective defense without a standing army, which the founding generation regarded as an instrument of tyranny. The militia system — the armed citizenry organized under state authority — was the chosen solution, and the amendment protected that system from federal neglect or disarmament.
% FOUNDING_PROBLEM_CORROBORATION: Founding-era sources outside any modern beneficiary set — state ratification debates and the anti-standing-army papers of The Federalist — attest the original problem. Military historians, also outside the beneficiary set, attest that the militia mechanism itself has been absorbed by a standing army and an institutionalized Guard. No party outside the benefiting apparatus attests that the original problem persists in its original militia-dependent form; the live contest is between 'the security problem persists, the mechanism changed' and 'both the problem-as-framed and the mechanism are gone.'
narrative_ontology:disappearance_verdict(second_amendment_text__collective_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(second_amendment_text__collective_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(second_amendment_text__collective_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(second_amendment_text__collective_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(second_amendment_text__collective_security_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(second_amendment_text__collective_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(second_amendment_text__collective_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(second_amendment_text__collective_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52 at interval end) is moderate and non-monotonic: it rose from 0.40 (1934) as federal and state regulation expanded past militia-related purposes — the 1968 act and the 1990s permit regimes restricted arms with no militia relationship — then fell after 2008 as federal enforcement retrenched, leaving state-level operation. Suppression (0.55) tracks the enforcement machinery's build-and-decay: it is the state's coercive perimeter (criminal penalties, permit denial), a raw structural property not scaled by power or scope, and the reading sees much of it as the legitimate price of the condition it endorses. Theater (0.50) is the clearest drift: the condition names 'organized civic defense,' but general militia service atrophied through the interval, so an increasing share of the arrangement's justification is performed rather than practiced — the National Guard is a real referent but a thinning one for a condition applied to the whole arms-owning population. Accessibility collapse is low (0.45): rival understandings of the text never collapsed and ultimately prevailed federally. Resistance (0.62) was sustained and organized across the interval. All series share one six-point grid (1934/1954/1974/1994/2008/2024); every tracked metric is authored at every point, with no metric-specific grids.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience this arrangement as civic architecture: the apparatus administers a mandate, the Guard inherits a constitutional anchoring, the public receives security provision. The payer seats experience the same structure as conditioned subordination: gun owners pay for and comply with a condition they do not administer, and unorganized-militia members carry an obligation whose organizing structure has decayed. The excluded seat experiences definitional foreclosure — no reorganization of their own conduct gains them standing. The engine computes these per-seat classifications from the power, exit, and role data; this commentary does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The apparatus sits nearest the beneficiary pole: it writes the conditions, collects the fees, and funds enforcement from the mandate (d near 0.0). The Guard is a beneficiary without administration — it collects constitutional anchoring and the civic role the condition names (low d). The general public is a diffuse beneficiary with a payer overlay: it receives the security provision and funds the apparatus while sitting inside the regulatory perimeter (low-to-moderate d). Gun owners are the primary target: they bear the transfer under constrained exit — relocation is costly, the federal floor follows them, and arms are durable, identity-laden goods (high d). Unorganized-militia members are the most trapped target: membership attaches by statute without consent, and the structure their obligation names has atrophied (d nearest 1.0 among the seats). Private militia organizers are excluded rather than positioned — the condition defines them out of the structure entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — collective defense without a standing army — has been transformed rather than solved: a standing army exists, and the Guard institutionalized the militia function. The arrangement persists while its naming justification ('organized civic defense' conditioning private arms) increasingly describes a category rather than a practice. Classification as tangled_rope is what keeps the mandatrophy question honest: it preserves the genuine coordination core (the Guard's anchoring, subordination of armed force to civil authority, standardized conditioning of access) instead of reading the whole arrangement as pure extraction, while the rising theater ratio and the decoupling of regulation from militia purpose mark the drift the omega variables track. The founding-problem status is authored contested — collective security remains live even as the militia mechanism is obsolete — so no dead-mandate verdict is asserted; the regulatory_apparatus_self_perpetuation omega is the instrument that would resolve it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading of the second_amendment_text kernel — the collective_security_reading. How much of the authored structure (beneficiary/victim sets, ε, classification) is contingent on that reading rather than on the underlying text?',
    'Compare the sibling-reading files (individual_right_reading, originalist_civic_virtue_reading): where their beneficiary/victim structures invert this one''s — gun owners as beneficiaries, the regulatory apparatus as the constrained seat — the structure is reading-indexed, not text-indexed.',
    'If the individual_right_reading governs, this story''s directionality map inverts and its classification does not transfer; the corpus must treat the three readings as separate constraints, never averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Reading-indexed contingency of the entire structural surface on the collective-security reading.').

omega_variable(
    prefatory_clause_interpretive_force,
    'The disagreement between readings is located in one structural element: does the prefatory clause (''A well regulated Militia...'') condition the operative clause''s right, or merely announce a purpose without limiting it?',
    'Doctrinal adjudication of the clause relationship — the specific interpretive move (conditioning vs. announcement) is the pivot on which the victim set and ε turn.',
    'If the clause merely announces purpose, this reading''s conditioned-right arrangement collapses into the individual-right arrangement and the regulatory apparatus loses its beneficiary position.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prefatory_clause_interpretive_force, conceptual, 'Location of the kernel contest in the prefatory clause''s interpretive force.').

omega_variable(
    militia_institution_referent,
    'Does the ''organized civic defense'' condition have a live institutional referent (the National Guard and state defense forces), or is the referent vestigial — a category maintained in statute while the practice it names has atrophied?',
    'Institutional data: militia enrollment, the Guard''s arms-custody relationship to its members, and which statutory militia obligations are actually enforced.',
    'If vestigial, the theater_ratio is understated and the arrangement drifts toward performance-maintained operation — a condition conditioning rights on a structure that no longer exists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militia_institution_referent, empirical, 'Whether the militia referent of the condition is live or vestigial.').

omega_variable(
    post_heller_operative_persistence,
    'After the 2008 federal repudiation, does this arrangement persist as operative law (state constitutional provisions, militia clauses, administrative structures), or only as legacy residue awaiting attrition?',
    'Survey state constitutional law and enforcement practice: count jurisdictions where militia-relationship reasoning still decides cases and licensing regimes still operate under collective-security justification.',
    'If legacy residue, the interval''s end-state values overstate present operation — current measurements describe an arrangement in terminal decay rather than live enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_heller_operative_persistence, empirical, 'Whether the arrangement operates post-repudiation or persists as residue.').

omega_variable(
    regulatory_apparatus_self_perpetuation,
    'Do the licensing and permit regimes serve the collective-security purpose the condition names, or has the apparatus''s self-maintenance (fee revenue, enforcement budgets, administrative continuity) become an independent driver?',
    'Cost-benefit audits of licensing regimes against measured security outcomes; compare jurisdictions with and without permit requirements on those outcomes.',
    'If self-maintenance dominates, the extraction measured here is bureaucratic rent riding on the condition — pushing the arrangement''s operation toward pure extraction and strengthening the case that the founding problem is dead.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_apparatus_self_perpetuation, empirical, 'Whether the apparatus serves the named purpose or self-perpetuates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(second_amendment_text__collective_security_reading, 1934, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(second_amendment_csr_tr_t1934, second_amendment_text__collective_security_reading, theater_ratio, 1934, 0.2).
narrative_ontology:measurement(second_amendment_csr_tr_t1954, second_amendment_text__collective_security_reading, theater_ratio, 1954, 0.24).
narrative_ontology:measurement(second_amendment_csr_tr_t1974, second_amendment_text__collective_security_reading, theater_ratio, 1974, 0.33).
narrative_ontology:measurement(second_amendment_csr_tr_t1994, second_amendment_text__collective_security_reading, theater_ratio, 1994, 0.4).
narrative_ontology:measurement(second_amendment_csr_tr_t2008, second_amendment_text__collective_security_reading, theater_ratio, 2008, 0.46).
narrative_ontology:measurement(second_amendment_csr_tr_t2024, second_amendment_text__collective_security_reading, theater_ratio, 2024, 0.5).

% Extraction over time
narrative_ontology:measurement(second_amendment_csr_be_t1934, second_amendment_text__collective_security_reading, base_extractiveness, 1934, 0.4).
narrative_ontology:measurement(second_amendment_csr_be_t1954, second_amendment_text__collective_security_reading, base_extractiveness, 1954, 0.42).
narrative_ontology:measurement(second_amendment_csr_be_t1974, second_amendment_text__collective_security_reading, base_extractiveness, 1974, 0.5).
narrative_ontology:measurement(second_amendment_csr_be_t1994, second_amendment_text__collective_security_reading, base_extractiveness, 1994, 0.58).
narrative_ontology:measurement(second_amendment_csr_be_t2008, second_amendment_text__collective_security_reading, base_extractiveness, 2008, 0.55).
narrative_ontology:measurement(second_amendment_csr_be_t2024, second_amendment_text__collective_security_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(second_amendment_csr_su_t1934, second_amendment_text__collective_security_reading, suppression_requirement, 1934, 0.45).
narrative_ontology:measurement(second_amendment_csr_su_t1954, second_amendment_text__collective_security_reading, suppression_requirement, 1954, 0.48).
narrative_ontology:measurement(second_amendment_csr_su_t1974, second_amendment_text__collective_security_reading, suppression_requirement, 1974, 0.58).
narrative_ontology:measurement(second_amendment_csr_su_t1994, second_amendment_text__collective_security_reading, suppression_requirement, 1994, 0.66).
narrative_ontology:measurement(second_amendment_csr_su_t2008, second_amendment_text__collective_security_reading, suppression_requirement, 2008, 0.6).
narrative_ontology:measurement(second_amendment_csr_su_t2024, second_amendment_text__collective_security_reading, suppression_requirement, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(second_amendment_text__collective_security_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__individual_right_reading).
narrative_ontology:affects_constraint(second_amendment_text__collective_security_reading, second_amendment_text__originalist_civic_virtue_reading).

% DUAL FORMULATION NOTE:
% The second_amendment_text kernel decomposes into three structurally distinct constraints — one per reading — because the readings assign different beneficiary/victim sets and different ε to the same text (ε-invariance: one reading, one ε). This file is the collective_security_reading. Influence across the family ran in both directions historically: this reading was upstream through the Miller era, when its militia-relationship test governed federal doctrine; the individual_right_reading became upstream after 2008 and now shapes this reading's operating environment by constraining where militia-relationship reasoning may still govern. Each family file links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
