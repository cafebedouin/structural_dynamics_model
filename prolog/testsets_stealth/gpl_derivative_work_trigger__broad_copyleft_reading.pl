% ============================================================================
% CONSTRAINT STORY: gpl_derivative_work_trigger__broad_copyleft_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gpl_derivative_work_trigger__broad_copyleft_reading, []).

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
 *   constraint_id: gpl_derivative_work_trigger__broad_copyleft_reading
 *   human_readable: GPL Broad Copyleft Linking Trigger (Broad Reading)
 *   domain: legal/technological
 *
 * SUMMARY:
 *   The GNU GPL's scope question — whether code that merely links to GPL code
 *   becomes a derivative work — is the central unresolved contest in copyleft
 *   licensing. This story instantiates the broad reading: any linking, static
 *   or dynamic, makes the combined work derivative, so distributing it
 *   triggers the GPL's source-disclosure obligations for the whole work.
 *   Under this reading the arrangement maintains the boundary of a
 *   reciprocity commons: the free software community and downstream
 *   recipients gain enforced reciprocation, while proprietary vendors face a
 *   disclose-or-avoid decision on every product that touches GPL components.
 *   No US court has definitively adopted or rejected the trigger; German
 *   litigation (Hellwig v. VMware) and US enforcement campaigns (BusyBox,
 *   2007-2011) have tested its edges, and the reading persists as an
 *   operative compliance constraint sustained by the FSF's interpretive
 *   authority, enforcement organizations, and vendor risk-aversion. Per the
 *   claim/metric independence rule, the claimed type records what I judge
 *   structurally true and the metrics record actual operation; any divergence
 *   between the claim and the engine's computed per-seat types is the
 *   measurement the corpus exists to take.
 *
 * KEY AGENTS:
 *   - free_software_community: primary beneficiary (organized / identity_locked) — collects enforced reciprocation; identity-fused with the copyleft commitment
 *   - downstream_recipients_of_source: beneficiary (powerless / mobile) — receives disclosed source; the in-kind receipt seat
 *   - copyleft_project_maintainers: beneficiary (moderate / mobile) — hold copyright, can relicense or dual-license
 *   - proprietary_software_vendors: primary target (institutional / constrained) — bears disclose-or-avoid cost on every GPL-linked product
 *   - embedded_device_manufacturers: target (organized / trapped) — historical firmware shipments create standing violations
 *   - fsf_license_stewards: agenda setter (organized / identity_locked) — publishes the text, articulates the broad reading, holds anchor copyrights
 *   - gpl_enforcement_organizations: agenda setter + secondary beneficiary (organized / identity_locked) — runs enforcement, collects settlements
 *   - end_users_of_noncompliant_products: excluded (powerless / trapped) — entitled to source, absent from every settlement table
 *   - software_law_scholars: analytical observer (moderate / analytical) — maps the doctrinal contest
 *   - courts_and_legislatures: observer (institutional / analytical) — would settle the trigger question; has so far avoided it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, 0.58).
domain_priors:suppression_score(gpl_derivative_work_trigger__broad_copyleft_reading, 0.55).
domain_priors:theater_ratio(gpl_derivative_work_trigger__broad_copyleft_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(gpl_derivative_work_trigger__broad_copyleft_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gpl_derivative_work_trigger__broad_copyleft_reading, tangled_rope).
narrative_ontology:human_readable(gpl_derivative_work_trigger__broad_copyleft_reading, "GPL Broad Copyleft Linking Trigger (Broad Reading)").
narrative_ontology:topic_domain(gpl_derivative_work_trigger__broad_copyleft_reading, "legal/technological").

domain_priors:requires_active_enforcement(gpl_derivative_work_trigger__broad_copyleft_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gpl_derivative_work_trigger__broad_copyleft_reading, 'ab30b986-ccd6-4530-9161-0f598e6fa007').
narrative_ontology:cs_kernel_codification('ab30b986-ccd6-4530-9161-0f598e6fa007', fixed_text).
narrative_ontology:cs_authority_grounding('ab30b986-ccd6-4530-9161-0f598e6fa007', lineage).
narrative_ontology:cs_interpretation_layer_present('ab30b986-ccd6-4530-9161-0f598e6fa007').
narrative_ontology:cs_reading_relation('ab30b986-ccd6-4530-9161-0f598e6fa007', gpl_derivative_work_trigger__narrow_linking_permissive_reading, forecloses).
narrative_ontology:cs_reading_relation('ab30b986-ccd6-4530-9161-0f598e6fa007', gpl_derivative_work_trigger__interface_boundary_reading, forecloses).
narrative_ontology:cs_axiom('ab30b986-ccd6-4530-9161-0f598e6fa007', foundational, linking_creates_derivative_work).
narrative_ontology:cs_axiom_status(linking_creates_derivative_work, holdable).
narrative_ontology:cs_axiom_grounding('ab30b986-ccd6-4530-9161-0f598e6fa007', linking_creates_derivative_work, conventional).
narrative_ontology:cs_axiom('ab30b986-ccd6-4530-9161-0f598e6fa007', secondary, copyleft_defense_extends_to_dependent_works).
narrative_ontology:cs_axiom_status(copyleft_defense_extends_to_dependent_works, holdable).
narrative_ontology:cs_axiom_grounding('ab30b986-ccd6-4530-9161-0f598e6fa007', copyleft_defense_extends_to_dependent_works, deontological).
narrative_ontology:cs_reference_frame('ab30b986-ccd6-4530-9161-0f598e6fa007', copyleft_reciprocity_commons).
narrative_ontology:cs_drift_state('ab30b986-ccd6-4530-9161-0f598e6fa007', contemporary_saas_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('ab30b986-ccd6-4530-9161-0f598e6fa007', '').
narrative_ontology:cs_kernel_id(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_community).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_project_maintainers).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_recipients_of_source).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors).
narrative_ontology:constraint_victim(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_device_manufacturers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_reciprocity_principle).
narrative_ontology:constraint_vindicates(gpl_derivative_work_trigger__broad_copyleft_reading, broad_derivative_work_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developers and users who write, maintain, and run GPL-licensed programs. The license's scope doctrine decides whether their collectively built infrastructure stays under community terms when others build on it. Copyleft commitment is constitutive of the community's self-understanding: abandoning the license framework would mean abandoning what the projects are, so exit is not a live option even where relicensing is legally possible.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, free_software_community, beneficiary,
    organized, generational, identity_locked, global).

% Users, integrators, and developers who receive binaries built on GPL code. Under the broad reading they are entitled to the complete corresponding source of the combined work, which they can study, modify, and rebuild. They bear no obligation unless they redistribute, and the source they receive is the in-kind form in which the obligation's payment arrives.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_recipients_of_source, beneficiary,
    powerless, biographical, mobile, global).

% Individuals and teams holding copyright on GPL libraries and applications. The broad scope reading shields their code from absorption into proprietary products without reciprocation. Because they hold copyright, they can relicense, dual-license, or sell commercial exceptions, so renegotiation and exit are available to them in ways they are not to the community at large; some fund their work by selling exactly the exemptions the obligation would otherwise forbid.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, copyleft_project_maintainers, beneficiary,
    moderate, biographical, mobile, global).

% Companies shipping proprietary products that link to GPL components, usually dynamically, since glibc and other core libraries sit beneath most Linux-based software. Compliance means releasing source they treat as trade secret, rearchitecting to remove the linkage, or substituting permissively licensed components. They contest the scope reading doctrinally, fund amicus positions against it, and maintain compliance departments whose entire function exists because of this obligation. Leaving the Linux ecosystem entirely is rarely economic; the realistic choices are pay, avoid, or litigate.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, proprietary_software_vendors, payer,
    institutional, biographical, constrained, global).

% Makers of routers, set-top boxes, televisions, and industrial devices whose firmware shipped with GPL-linked code and incomplete or missing source offers. Their violations are historical facts baked into distributed hardware; re-releasing source after supply chains close is costly and sometimes impossible. They are the recurring defendants in enforcement actions and have the least room to restructure around the obligation.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, embedded_device_manufacturers, payer,
    organized, immediate, trapped, global).

% The Free Software Foundation publishes the GPL text, maintains FAQ positions including the broad linking position, and holds copyright on core utilities that anchor enforcement leverage. Its authority rests on continuity with the license's founding text and tradition; retreating from the broad reading would repudiate its own founding commitments, so the position is not revisable without dissolving the steward's role.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, fsf_license_stewards, agenda_setter,
    organized, generational, identity_locked, global).

% Organizations such as the Software Freedom Conservancy and gpl-violations.org that run compliance campaigns and litigation on behalf of copyright holders. They collect settlement funds and build the enforcement record that gives the broad reading practical force. Their operating model depends on the obligation staying broad enough to generate violations to pursue, and their identity is bound to the enforcement mission.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations, agenda_setter,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_enforcement_organizations, beneficiary).

% Owners of shipped devices and products that contained GPL code without complete source offers. The disclosure they were entitled to mostly never arrives: enforcement settles between copyright holders and vendors, and individual users lack standing, resources, and notice. Their claims are what the obligation exists to satisfy, and they are the least present at any table where compliance is negotiated.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, end_users_of_noncompliant_products, excluded,
    powerless, immediate, trapped, global).

% Academic copyright lawyers who map the derivative-work doctrine and the copyleft scope debate. They produce the doctrinal analyses that vendor counsel and enforcement organizations alike cite. They hold no stake in the code, their assessments bind no one, and the contest between the readings is carried substantially in their literature.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, software_law_scholars, observer,
    moderate, generational, analytical, national).

% Adjudicators whose eventual ruling on linking and derivative-work status would settle the contest the sibling readings institutionalize. To date they have resolved enforcement cases on standing, procedure, and settlement without reaching the trigger question, leaving the reading's legal status open and the interpretive layer unreviewed by any binding counterweight.
narrative_ontology:constraint_stakeholder(gpl_derivative_work_trigger__broad_copyleft_reading, courts_and_legislatures, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gpl_derivative_work_trigger__broad_copyleft_reading, downstream_recipients_of_source).
narrative_ontology:fixing_cost_class(gpl_derivative_work_trigger__broad_copyleft_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the boundary of a reciprocity commons: code published under the GPL stays available to everyone who accepts the same terms, and the disclosure obligation makes building on the commons and contributing back the same act. Without a scope doctrine reaching linked works, vendors could take the commons' infrastructure (compilers, libc, kernels, utilities), improve it, and ship the improvements closed, draining the shared resource.
% TRANSFER_FUNCTION: Moves complete corresponding source code, and the freedoms to study, modify, and redistribute, from vendors who distribute GPL-linked binaries to downstream recipients and the commons. Moves compliance cost (disclosure, reengineering, or component substitution) onto proprietary vendors. Moves settlement payments to enforcing copyright holders and organizations when violations are litigated rather than cured in kind.
% ABSENT_VOICES: End users of noncompliant products would claim the source they were denied but are not parties to enforcement, which settles between copyright holders and vendors. Vendors relying in good faith on the narrow or interface readings are absent from the FSF's FAQ process that articulates this reading; their position lives only in the sibling readings and amicus briefs. No court participates in the interpretive layer, so the process that absorbs doctrinal drift includes no binding counterweight.
% DISAPPEARANCE_RATIONALE: If the broad reading disappeared overnight, vendors could link GPL code and ship without disclosure: proprietary adoption of GPL libraries would surge, projects would lose the disclose-or-avoid lever that funds dual licensing and enforcement, the commons boundary would contract to modification-only, and vendors that reengineered around GPL components would reverse those decisions. Arrangements across the Linux software economy are built around the obligation's existence.
% FOUNDING_PROBLEM: Proprietary enclosure of free software: through the 1980s and early 1990s, vendors took freely shared code, improved it, and released the improvements as proprietary products, draining the commons. Copyleft, and the scope doctrine this reading instantiates, was built so that building on free code obligates returning the build to the commons.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by: the historical record of pre-GPL proprietarization episodes; proprietary vendors' own compliance programs, which treat the obligation as a real legal risk and budget staff and tooling against it (attesting operative force, not justice); and academic copyright scholarship across the doctrinal spectrum, which takes enclosure-avoidance as the license's documented purpose even where individual scholars reject the broad trigger. No source outside the copyleft movement attests that the founding problem is dead.
narrative_ontology:disappearance_verdict(gpl_derivative_work_trigger__broad_copyleft_reading, world_rearranges).
narrative_ontology:founding_problem_status(gpl_derivative_work_trigger__broad_copyleft_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gpl_derivative_work_trigger__broad_copyleft_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gpl_derivative_work_trigger__broad_copyleft_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gpl_derivative_work_trigger__broad_copyleft_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gpl_derivative_work_trigger__broad_copyleft_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The scenario's expected delta describes a rope, but a rope requires all participants to be net beneficiaries; the vendor seats are not, so the structurally honest claim is the hybrid. Extractiveness is 0.58 at interval end: the disclosure burden is negligible for commons participants, who already publish source, and substantial for proprietary linkers, for whom compliance means releasing source treated as trade secret, rearchitecting to remove linkage, or substituting permissive components — concentrated on the vendor seats, moderate averaged over the governed set. Suppression is 0.55 and is structural rather than internalized: the reading forecloses one integration path (link and stay closed) through legal risk and enforcement, while leaving reimplementation, permissive substitutes, and commercial exceptions from dual-licensing copyright holders. Theater ratio is 0.22: enforcement is real (litigation, settlements, source releases) but a compliance-paperwork industry has grown around the obligation, and a share of enforcement activity defends the reading's scope rather than recovering source for users. Accessibility collapse is 0.48: once a vendor understands the reading, link-and-close collapses as an option, but workable alternatives persist. Resistance is 0.72: the reading meets organized doctrinal opposition — the sibling readings are that opposition institutionalized — plus industry amicus activity and deliberate avoidance. Identity-lock dynamics: the community and steward seats are identity_locked ideologically and institutionally — the copyleft commitment constitutes their self-concept and organizational mandate — so their position is stable under exit modulation; if that identity frame broke (the movement accepting permissive norms as coequal), the reading's enforcement base would collapse within a generation. The measurement series share one grid (1995-2025, seven points, three metrics): extraction rose through 2015 as Linux and glibc made GPL linkage unavoidable for vendors shipping Linux-based products and as enforcement professionalized, then eased slightly as SaaS migration and avoidance moved targets beyond the distribution trigger; the suppression requirement rose with enforcement professionalization and plateaued once enforcement capacity matured; theater crept up with compliance paperwork. Suppression is authored as a raw structural property; only extractiveness is scaled by the engine, via directionality and scope.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor seats the same structure operates as a costly enforced disclosure regime with doctrinally contested foundations; from the community and downstream seats it operates as the mechanism that keeps collectively built infrastructure reciprocated; from the enforcement organizations it is an operating mandate. The agenda-setter seat is genuinely dual: the FSF administers a reading it also lives by, and enforcement organizations administer it while collecting settlements. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: the free software community (identity-locked, so its low d is stable), maintainers (mobile exit via relicensing), and downstream recipients (mobile, diffuse). Victims map to high d: proprietary vendors (constrained exit — switching costs and installed bases make avoidance expensive but possible) and embedded device manufacturers (trapped — historical shipments create violations that cannot be undone). The derivation from beneficiary/victim declarations plus exit options yields the right relationships for every seat, so no directionality overrides are authored. The enforcement organizations' dual position is carried by the role pair rather than an override; the excluded seat (end users of noncompliant products) feeds the absent-voices record, not classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — proprietary enclosure of distributed free software — remains live for distributed software, so this is not a resolved mandate. The SaaS migration erodes the constraint's reach at the margin (network delivery triggers no distribution under any reading of this kernel), which is tracked in the saas_migration_reach_decay omega rather than declared as mandatrophy. Classifying the reading as a hybrid prevents two misreadings: calling it pure coordination would erase the concentrated compliance burden on vendor seats; calling it pure extraction would erase the genuine commons function that downstream recipients actually collect. The R5 fields record status=live with a disappearance verdict of world_rearranges, so no dead-mandate mismatch flag is expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_reading_selection,
    'This constraint instantiates the broad_copyleft_reading of kernel gpl_derivative_work_trigger; would instantiating a sibling reading instead change the constraint''s structural identity?',
    'Author the sibling files and compare: narrow_linking_permissive_reading collapses the victim set to modifiers of GPL source only; interface_boundary_reading exempts vendors coupling across clean APIs, shrinking both the victim set and the enforcement surface.',
    'Under either sibling, the cost asymmetry narrows and the classification plausibly shifts toward a thinner coordination arrangement; the disagreement between the readings is located in the derivative-work test for linked code, not in the value of copyleft itself.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_reading_selection, conceptual, 'Kernel-reading selection: which trigger test this constraint embodies, and what the sibling readings would change structurally.').

omega_variable(
    derivative_work_doctrinal_status,
    'Does linking, static or dynamic, actually create a derivative work as a matter of copyright law?',
    'A definitive appellate ruling or legislative clarification on the derivative-work status of linked code; German case law and US enforcement settlements so far resolve standing and procedure without reaching the trigger question.',
    'If the narrow position prevails doctrinally, this reading''s victim set collapses to source modifiers and its operative force decays toward inertia; if the broad position is confirmed, vendor-seat costs intensify and enforcement widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(derivative_work_doctrinal_status, empirical, 'Unresolved legal status of the linking-as-derivation claim under copyright doctrine.').

omega_variable(
    saas_migration_reach_decay,
    'Is the reading''s effective reach decaying as software delivery migrates to network services that trigger no distribution?',
    'Track AGPL adoption rates, GPL-library avoidance by service builders, and whether copyleft stewards extend the trigger to network use.',
    'If migration continues, the distribution-bound reading persists as enforced formality over a shrinking domain while a network-use trigger becomes the live obligation surface; the temporal series would show extraction decoupling from reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saas_migration_reach_decay, empirical, 'Cloud and SaaS deployment escaping the distribution trigger that this reading rides on.').

omega_variable(
    enforcement_standing_fragmentation,
    'Who may enforce the disclosure obligation: every contributor, only substantial copyright holders, or designated organizations?',
    'Outcomes of standing disputes (the German VMware litigation turned on the plaintiff''s share of the copyrighted work) and whether enforcement consolidates in resourced organizations.',
    'Fragmented standing raises the enforcement effort needed to hold the broad reading and narrows effective enforcement to well-resourced organizations; consolidated standing would lower enforcement cost and widen reach to individual contributors'' claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_standing_fragmentation, empirical, 'Standing fragmentation constrains who can hold the reading operative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gpl_derivative_work_trigger__broad_copyleft_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpl_broad_reading_tr_t1995, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(gpl_broad_reading_tr_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(gpl_broad_reading_tr_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(gpl_broad_reading_tr_t2010, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(gpl_broad_reading_tr_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(gpl_broad_reading_tr_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2020, 0.21).
narrative_ontology:measurement(gpl_broad_reading_tr_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(gpl_broad_reading_be_t1995, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 1995, 0.38).
narrative_ontology:measurement(gpl_broad_reading_be_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2000, 0.44).
narrative_ontology:measurement(gpl_broad_reading_be_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(gpl_broad_reading_be_t2010, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(gpl_broad_reading_be_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2015, 0.62).
narrative_ontology:measurement(gpl_broad_reading_be_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2020, 0.6).
narrative_ontology:measurement(gpl_broad_reading_be_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(gpl_broad_reading_su_t1995, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(gpl_broad_reading_su_t2000, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2000, 0.45).
narrative_ontology:measurement(gpl_broad_reading_su_t2005, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2005, 0.5).
narrative_ontology:measurement(gpl_broad_reading_su_t2010, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2010, 0.54).
narrative_ontology:measurement(gpl_broad_reading_su_t2015, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(gpl_broad_reading_su_t2020, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2020, 0.55).
narrative_ontology:measurement(gpl_broad_reading_su_t2025, gpl_derivative_work_trigger__broad_copyleft_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gpl_derivative_work_trigger__broad_copyleft_reading, identity_coordination).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__narrow_linking_permissive_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, gpl_derivative_work_trigger__interface_boundary_reading).
narrative_ontology:affects_constraint(gpl_derivative_work_trigger__broad_copyleft_reading, agpl_network_use_trigger).

% DUAL FORMULATION NOTE:
% The colloquial label 'GPL linking obligations' decomposes into three structurally distinct readings of one kernel, the derivative-work trigger: this broad reading (all linking triggers disclosure), narrow_linking_permissive_reading (only modification triggers), and interface_boundary_reading (clean API boundaries are exempt even with tight coupling). They share the GPL text and the copyleft purpose but carry different epsilon values, different victim sets, and different enforcement surfaces, and they are mutually exclusive answers to a single doctrinal question, hence the forecloses edges. The AGPL network-use trigger is the downstream response constraint: it extends the copyleft boundary to the deployment mode (network service) that escapes every distribution-bound reading of this kernel, including this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
