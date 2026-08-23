% ============================================================================
% CONSTRAINT STORY: dignity_kernel__imago_dei_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignity_kernel__imago_dei_reading, []).

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
 *   constraint_id: dignity_kernel__imago_dei_reading
 *   human_readable: Imago Dei Dignity Regime: Divine-Image Worth Prior to Capability, Enforced Over Technology Governance
 *   domain: theological ethics/technology governance/philosophical anthropology
 *
 * SUMMARY:
 *   The imago Dei reading holds that every human person bears the inviolable
 *   image of the Triune God, with worth equal across all persons and prior to
 *   any capability — a standing that no deficit of cognition, productivity,
 *   or independence can forfeit and no augmentation can increase.
 *   Operatively, this reading governs technology: artificial intelligence
 *   must remain a tool subordinate to the human person; human enhancement and
 *   superintelligence trajectories are categorically rejected as violations
 *   of created order; and the declared victim set comprises any human
 *   subjected to technocratic reduction or transhumanist transformation. The
 *   regime is carried by trinitarian religious institutions that teach,
 *   enforce through formation and institutional policy, and advocate in
 *   legislatures and bioethics fora. This story instantiates ONE reading of
 *   the dignity_kernel; the autonomy_rights_reading and posthumanist_reading
 *   are separate constraints with their own epsilon values, victim sets, and
 *   enforcement structures, linked through network.affects_constraints. Per
 *   the epsilon-invariance principle, the epsilon referent here is the
 *   standing imago Dei doctrinal regime itself — as it actually operates,
 *   including its asymmetries — not the rights-respecting or transhumanist
 *   arrangements any other reading would install. Interval mapping: time
 *   points 0-30 correspond approximately to 1995-2025, the era in which the
 *   doctrine extended from pastoral care into explicit biotechnology and AI
 *   governance.
 *
 * KEY AGENTS:
 *   - - trinitarian_religious_institutions: Primary agenda-setter (institutional/identity_locked) — teaches the doctrine, sets technology boundaries for members, operates hospitals and schools under it, lobbies on bioethics and AI policy; collects authority, cohesion, and institutional identity from maintaining the teaching
 *   - - capability_poor_persons: Primary protected beneficiary (powerless/constrained) — infants, the severely cognitively disabled, the demented; receive unconditional standing independent of function
 *   - - doctrinal_community_members: Secondary beneficiary with payer aspect (organized/identity_locked) — lay faithful who receive meaning and community and pay in renounced options and formed-conscience self-limitation
 *   - - enhancement_seeking_persons: Payer (moderate/mobile) — persons whose augmentation desires are categorically condemned rather than regulated
 *   - - ai_development_community: Payer (powerful/arbitrage) — builders of capable AI systems working under a normative ceiling of permanent toolhood
 *   - - transhumanist_advocates: Payer (organized/constrained) — organized movements under categorical condemnation rather than debate
 *   - - secular_bioethics_bodies: Analytical observer (organized/analytical) — commissions and standards bodies adjudicating between this grounding and rivals
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, 0.62).
domain_priors:suppression_score(dignity_kernel__imago_dei_reading, 0.63).
domain_priors:theater_ratio(dignity_kernel__imago_dei_reading, 0.26).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, suppression_requirement, 0.63).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, theater_ratio, 0.26).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(dignity_kernel__imago_dei_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignity_kernel__imago_dei_reading, tangled_rope).
narrative_ontology:human_readable(dignity_kernel__imago_dei_reading, "Imago Dei Dignity Regime: Divine-Image Worth Prior to Capability, Enforced Over Technology Governance").
narrative_ontology:topic_domain(dignity_kernel__imago_dei_reading, "theological ethics/technology governance/philosophical anthropology").

domain_priors:requires_active_enforcement(dignity_kernel__imago_dei_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignity_kernel__imago_dei_reading, '74a63cbc-26e5-4d73-ac2a-d64da7839dc0').
narrative_ontology:cs_kernel_codification('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', fixed_text).
narrative_ontology:cs_authority_grounding('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', lineage).
narrative_ontology:cs_interpretation_layer_present('74a63cbc-26e5-4d73-ac2a-d64da7839dc0').
narrative_ontology:cs_reading_relation('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', dignity_kernel__autonomy_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', dignity_kernel__posthumanist_reading, forecloses).
narrative_ontology:cs_axiom('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', foundational, dignity_prior_to_capability).
narrative_ontology:cs_axiom_status(dignity_prior_to_capability, holdable).
narrative_ontology:cs_axiom_grounding('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', dignity_prior_to_capability, deontological).
narrative_ontology:cs_axiom('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', foundational, human_nature_fixed_created_limit).
narrative_ontology:cs_axiom_status(human_nature_fixed_created_limit, holdable).
narrative_ontology:cs_axiom_grounding('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', human_nature_fixed_created_limit, theological).
narrative_ontology:cs_axiom('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', secondary, artificial_intelligence_mere_instrument).
narrative_ontology:cs_axiom_status(artificial_intelligence_mere_instrument, holdable).
narrative_ontology:cs_axiom_grounding('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', artificial_intelligence_mere_instrument, theological).
narrative_ontology:cs_reference_frame('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', created_order_human_summit).
narrative_ontology:cs_drift_state('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', contemporary_enhancement_era, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('74a63cbc-26e5-4d73-ac2a-d64da7839dc0', '').
narrative_ontology:cs_kernel_id(dignity_kernel__imago_dei_reading, dignity_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, trinitarian_religious_institutions).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, capability_poor_persons).
narrative_ontology:constraint_beneficiary(dignity_kernel__imago_dei_reading, doctrinal_community_members).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, enhancement_seeking_persons).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, ai_development_community).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, transhumanist_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignity_kernel__imago_dei_reading, doctrinal_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Teach the doctrine of the divine image, set the boundaries of permissible technology for their members, operate hospitals and schools under doctrinal ethics directives, and lobby legislatures and bioethics bodies on enhancement and AI questions. Authority, membership cohesion, and institutional purpose flow from maintaining the teaching; enforcement runs through formation, sacramental and institutional discipline, and policy. Abandoning the teaching would dissolve the institution's reason for being, so exit is not a live option from where they stand.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, trinitarian_religious_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Infants, people with severe cognitive disability, advanced dementia, and profound dependence receive unconditional standing under the teaching: their worth is asserted as independent of anything they can do. Care and advocacy flow to them through doctrinal institutions — hospitals, residential communities, pro-life and disability ministries. They cannot opt out of being protected, and protection regimes outside the tradition tend to be weaker or conditional on projected recovery or social usefulness.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, capability_poor_persons, beneficiary,
    powerless, biographical, constrained, global).

% Lay faithful who accept the teaching receive meaning, community, moral clarity, and the assurance of unconditional worth for themselves and their dependents. They also renounce enhancement options and treat the subordination of AI tools as settled matters of formed conscience. Belonging is fused with identity — leaving would mean losing community and self-understanding together — so most never seriously weigh departure, and desires that conflict with the teaching are experienced as temptations to be resisted rather than grievances to be aired.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, doctrinal_community_members, beneficiary,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(dignity_kernel__imago_dei_reading, doctrinal_community_members, payer).

% People who want cognitive, physical, or longevity augmentation find the object of their desire condemned outright rather than regulated. Their options are travel to permissive jurisdictions, gray markets of uncertain safety, or waiting out the doctrine's cultural power. The costs are illegality in some places, unsafe supply where legal channels are closed, and moral condemnation from fellow citizens whose institutions hold the teaching.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, enhancement_seeking_persons, payer,
    moderate, biographical, mobile, global).

% Builders of increasingly capable AI systems work under a normative ceiling that declares their artifacts must remain tools and forbids trajectories toward machine persons or superintelligence. Research agendas, funding relationships, and public legitimacy partially route through institutions that hold this line. Researchers and firms can relocate to jurisdictions and capital pools indifferent to the doctrine, at some cost in partnerships and market access where the teaching shapes regulation.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, ai_development_community, payer,
    powerful, generational, arbitrage, global).

% Organized movements promoting human enhancement, radical life extension, and digital continuation of persons operate under categorical condemnation rather than invitation to debate. Their projects lose legitimacy, funding channels, and political access wherever the teaching holds sway; they respond by building counter-institutions and shopping jurisdictions. Their commitments are generational — the movement expects to outlast current opposition — but within doctrine-shaped societies their exit is limited to margins.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, transhumanist_advocates, payer,
    organized, generational, constrained, global).

% Governmental commissions, academic centers, and standards bodies adjudicate between the theological grounding of worth and its rivals. They take testimony from all sides, commission philosophical and empirical analysis, and issue guidance that determines how far the teaching's conclusions reach into law and clinical practice. Their seat is analytical: they neither collect from the teaching nor bear its costs, though their recommendations continuously reshape its operational territory.
narrative_ontology:constraint_stakeholder(dignity_kernel__imago_dei_reading, secular_bioethics_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignity_kernel__imago_dei_reading, trinitarian_religious_institutions).
narrative_ontology:fixing_cost_class(dignity_kernel__imago_dei_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains unconditional, capability-independent standing for every human person — solving the recurring collective-action problem of who counts when capacities vary wildly (infancy, dementia, severe disability) — and coordinates the community's shared refusal to sort persons by function, extended in the contemporary era to a shared line against machine persons and self-directed human redesign.
% TRANSFER_FUNCTION: Moves definitional authority over who and what possesses inviolable worth from markets, laboratories, and individual self-conception to the tradition's teaching office; moves option-space (enhancement, machine-personhood trajectories) away from seekers and builders, converting it into protected stability for the unmodified human category and institutional authority for the teacher.
% ABSENT_VOICES: Enhancement-desiring believers inside the tradition largely self-censor — formed conscience renders the desire as temptation rather than grievance, so the people most burdened by the prohibition rarely appear in the tradition's own discernment. Secular patients governed by doctrine-derived hospital policies and laws had no seat where the teaching was formed. And the enhanced persons the categorical ban forecloses cannot exist to object — the ban's most affected constituency is one it prevents from coming into being.
% DISAPPEARANCE_RATIONALE: If the regime vanished overnight, capability-sorting pressures would rapidly reorganize care, insurance, and labor around functional worth: triage and long-term care would price cognition and prognosis directly, the enhancement arms race would proceed without a categorical brake, AI development would lose its principal normative ceiling, and the religious communities carrying the teaching would lose a core boundary marker and a major strand of their social identity. The rearrangement would be fastest exactly where the doctrine currently binds hardest.
% FOUNDING_PROBLEM: Protecting persons whose capacities make them economically or socially discountable — infants, the demented, the severely disabled — from being sorted by function; articulated theologically as guarding the divine image present in every person against reduction to capability, utility, or self-possession.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights advocates and secular inherent-dignity instruments (the UDHR preamble's 'inherent dignity' language descends from this tradition) attest the founding problem from outside the benefiting parties; bioethics literature documenting cognitive-market stratification and AI-driven valuation of persons corroborates that the pressure is intensifying rather than fading. Note the division of testimony: the problem's liveness is corroborated by these outside sources, but whether THIS theological solution remains necessary — as against secular equivalents — is disputed by those same sources, several of which affirm the problem while rejecting the doctrine's authority structure.
narrative_ontology:disappearance_verdict(dignity_kernel__imago_dei_reading, world_rearranges).
narrative_ontology:founding_problem_status(dignity_kernel__imago_dei_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignity_kernel__imago_dei_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignity_kernel__imago_dei_reading, 'none', 1).
narrative_ontology:epsilon_provenance(dignity_kernel__imago_dei_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignity_kernel__imago_dei_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignity_kernel__imago_dei_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignity_kernel__imago_dei_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.62: the categorical prohibitions impose real, concentrated option-loss on identifiable minorities (enhancement seekers, AI builders, transhumanists) while the diffuse majority receives protection and meaning at little personal cost — a classic asymmetric structure, though the transferred good is option-space and definitional authority rather than money. Suppression at 0.63 reflects enforcement that is real but predominantly non-state: formation, sacramental and institutional discipline, doctrinal hospital policy, and legislative advocacy; it relies heavily on voluntary assent and identity, which is why it does not approach coercive maxima. Theater ratio at 0.26: most activity is functional (care delivery, teaching, policy work); a growing minority is ritual reaffirmation — condemnations of technologies already excluded internally, issued for boundary-marketing rather than behavior change. Accessibility_collapse at 0.40: alternatives persist robustly (secular bioethics, permissive jurisdictions, autonomy-grounded frameworks), so the regime collapses options only within its own jurisdiction rather than globally. Resistance at 0.60: an organized transhumanist counter-movement, a powerful and largely indifferent AI sector, secular bioethical rivalry, and internal dissent from enhancement-sympathetic theologians. The temporal series run on one shared grid (points 0,6,12,18,24,30 for all three tracked metrics). Rising base_extractiveness traces the doctrine's extension into successive technology domains (reproductive tech, genomics, AI) — each extension converts previously unregulated option-space into prohibited space. Rising suppression_requirement traces deliberate enforcement build-out: bioethics commissions, ethics directives for institutional healthcare, denominational AI statements, lobbying infrastructure. Mildly rising theater_ratio traces ritual maintenance accumulating atop stable practice. Suppression_requirement is tracked because enforcement-capacity change is precisely the dynamic this story narrates; the trajectory is build-up, not decay.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structure. From the institution's seat, the arrangement is protective coordination it has stewarded for centuries: the equal-worth floor is its proudest social contribution, and the technology prohibitions are fidelity to created order. From the enhancement seeker's and transhumanist's seats, the same structure is categorical foreclosure enforced by an authority whose premises they do not share — coordination for insiders purchased with prohibition for outsiders. From the capability-poor person's seat, it is a lifeline: the only standing that has never been conditional on their function. From the AI builder's seat, it is a normative ceiling imposed by a minority tradition with disproportionate institutional leverage. The engine derives these divergent per-seat classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Trinitarian_religious_institutions sit at the beneficiary pole (d near 0.0): they administer the regime and collect authority, cohesion, and identity from it, with identity-locked exit amplifying their subsidy. Capability_poor_persons are full beneficiaries (low d) — the regime's protections flow entirely toward them and they bear essentially none of its costs. Doctrinal_community_members derive low-to-symmetric d: genuine receipt of meaning and protection, offset by renounced options borne willingly. Enhancement_seeking_persons derive high d (near-target), damped by mobile exit — jurisdictional arbitrage softens but does not remove their exposure. Ai_development_community derive high d damped further by arbitrage-grade exit (capital and talent relocate easily). Transhumanist_advocates derive near-full-target d: condemned categorically, with only constrained exit through counter-institution-building. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms produce accurate d for every seat, and the dual-positioned member seat is handled by its secondary_role rather than an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting persons whose capacities make them economically or socially discountable — remains live and is intensifying (AI-driven valuation of cognition, biotech stratification), so this is not a mandatrophy case and mandatrophy_resolved is not declared. The live risk is mandate EXTENSION: the AI-subordination and enhancement-prohibition clauses are far younger than the founding problem and ride on its accumulated authority, and the classification apparatus must not let the old problem's legitimacy silently underwrite the new prohibitions. Claiming tangled_rope (rather than snare) preserves visibility of the genuine coordination core — erasing it would mislabel real protection of the capability-poor as mere cover. Claiming tangled_rope (rather than rope) keeps the asymmetric prohibition layer flagged — blessing it as pure coordination would launder categorical foreclosure of outsiders as costless common good. Should the founding problem ever die (capability-sorting pressure vanishing), the predicted decay path is toward piton: ritual condemnations of moot technologies maintained by inertia, detectable via the theater_ratio trajectory crossing above 0.5 while extractiveness falls.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexical,
    'This constraint is ONE reading (imago_dei_reading) of the contested dignity_kernel; the sibling readings (autonomy_rights_reading, posthumanist_reading) instantiate different constraints. Does any classification reached here generalize to the kernel as a whole?',
    'Comparative read of the sibling stories'' compiled classifications; the kernel-level verdict is a cross-reading synthesis, never recoverable from this file alone.',
    'Every metric, victim set, and enforcement structure in this story is indexical to the imago Dei reading; importing them into kernel-level verdicts would fabricate consensus across readings that structurally disagree.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexical, conceptual, 'Committer-frame indexicality: this story''s verdict binds only to the imago Dei reading.').

omega_variable(
    sibling_delta_autonomy_rights,
    'What would the autonomy_rights_reading change structurally if instantiated instead?',
    'Author and compile the sibling story; compare victim sets, enforcement locus, and epsilon.',
    'Grounding shifts from divine conferral to autonomous rational capacity; the victim set narrows to rights-violations against persons (technocratic reduction of persons remains, enhancement prohibition weakens to consent-and-harm regulation); enforcement migrates from ecclesial discipline to constitutional law; capability-independence becomes derivative of personhood rather than constitutive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_delta_autonomy_rights, conceptual, 'Structural delta if the autonomy-grounded sibling reading were instantiated.').

omega_variable(
    disagreement_location_grounding,
    'Where exactly do the readings of dignity_kernel disagree?',
    'Locate the disputed element: the GROUND of worth — conferral by God prior to and independent of capability (this reading) versus constitution by autonomous capacity (autonomy reading) versus denial of any fixed limit (posthumanist reading).',
    'All downstream structure (AI subordination, categorical enhancement rejection, victim sets) follows from this single element; resolving the grounding dispute resolves the entire family''s divergence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_grounding, conceptual, 'The kernel dispute localizes in the ground of worth, not in its applications.').

omega_variable(
    divine_image_realism_vs_construction,
    'Is the equal-worth floor a discovered moral reality that would hold without enforcement, or a constructed doctrine whose persistence serves identifiable institutional interests?',
    'Cross-cultural convergence studies on capability-independent worth; natural experiments where enforcement capacity lapses (declining congregations, secularized healthcare systems) — does the protection norm survive without the enforcing institution?',
    'If the floor survives enforcement collapse, the coordination component is deeper than its current institutional carrier and the extraction component is separable from it; if it decays with the institution, the whole structure is closer to institutional self-maintenance than to discovered moral law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_image_realism_vs_construction, empirical, 'Whether the capability-independent worth floor is discovered or institutionally constructed.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression of enhancement-desire among the faithful structural (institutional gatekeeping, jurisdictional prohibition) or internalized (formed conscience that pre-empts the desire as temptation)?',
    'Post-exit suppression trajectory: track lapsed members'' enhancement-related choices after leaving the community; if restraint persists after gatekeeping ends, a large share is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — the enforcement burden on institutions is lower than it appears and the constraint travels with members beyond institutional reach.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism in the doctrinal community.').

omega_variable(
    protection_paternalism_tradeoff,
    'Do capability-poor persons net-benefit from the doctrine''s protection, or is the protection partly offset by paternalistic denial of agency (refusal of requested interventions, substituted judgment about their good)?',
    'Preference elicitation from affected persons themselves (where communicatively possible) and from proxy-consent records; compare outcomes under doctrinal versus consent-centered care regimes.',
    'If paternalistic offsets are large, the beneficiary declaration for capability_poor_persons overstates their position and their derived directionality should sit nearer symmetric than the structural data suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_paternalism_tradeoff, preference, 'Net-benefit ambiguity in the doctrine''s protective function for those it protects.').

omega_variable(
    tool_subordination_boundary,
    'Does the AI-as-mere-instrument clause extend to all machine cognition, or only to systems approaching person-like status — and where exactly is the line?',
    'Doctrinal analysis of the teaching office''s own applications (which systems have been treated as covered) combined with philosophical clarification of the personhood threshold the reading implies.',
    'A narrow reading shrinks the ai_development_community victim set to frontier-systems builders; a broad reading places the entire machine-intelligence sector under the prohibition and raises measured extractiveness accordingly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tool_subordination_boundary, conceptual, 'Boundary ambiguity in the tool-subordination clause''s coverage.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignity_kernel__imago_dei_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignity_kernel__imago_dei_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(dign_tr_t6, dignity_kernel__imago_dei_reading, theater_ratio, 6, 0.18).
narrative_ontology:measurement(dign_tr_t12, dignity_kernel__imago_dei_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(dign_tr_t18, dignity_kernel__imago_dei_reading, theater_ratio, 18, 0.22).
narrative_ontology:measurement(dign_tr_t24, dignity_kernel__imago_dei_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(dign_tr_t30, dignity_kernel__imago_dei_reading, theater_ratio, 30, 0.26).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignity_kernel__imago_dei_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement(dign_be_t6, dignity_kernel__imago_dei_reading, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(dign_be_t12, dignity_kernel__imago_dei_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement(dign_be_t18, dignity_kernel__imago_dei_reading, base_extractiveness, 18, 0.56).
narrative_ontology:measurement(dign_be_t24, dignity_kernel__imago_dei_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(dign_be_t30, dignity_kernel__imago_dei_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignity_kernel__imago_dei_reading, suppression_requirement, 0, 0.46).
narrative_ontology:measurement(dign_su_t6, dignity_kernel__imago_dei_reading, suppression_requirement, 6, 0.5).
narrative_ontology:measurement(dign_su_t12, dignity_kernel__imago_dei_reading, suppression_requirement, 12, 0.53).
narrative_ontology:measurement(dign_su_t18, dignity_kernel__imago_dei_reading, suppression_requirement, 18, 0.57).
narrative_ontology:measurement(dign_su_t24, dignity_kernel__imago_dei_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(dign_su_t30, dignity_kernel__imago_dei_reading, suppression_requirement, 30, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignity_kernel__imago_dei_reading, identity_coordination).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__autonomy_rights_reading).
narrative_ontology:affects_constraint(dignity_kernel__imago_dei_reading, dignity_kernel__posthumanist_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the dignity_kernel per the epsilon-invariance principle: the colloquial label 'human dignity' conflates three structurally distinct claims, here split into three stories. This story (imago_dei_reading) is the historically upstream member — the oldest articulation, from which the others define themselves partly in contrast. The autonomy_rights_reading is the dominant public-governance descendant (secularized inherent-dignity instruments); the posthumanist_reading is the explicit repudiator. Each carries its own epsilon, victim set, and enforcement structure; edges run from this story to both siblings because its premises are cited as authority by defenders in both disputes. Sibling files must carry reciprocal links and their own dual-formulation notes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
