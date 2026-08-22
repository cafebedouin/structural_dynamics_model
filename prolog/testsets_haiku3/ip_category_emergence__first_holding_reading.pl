% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__first_holding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__first_holding_reading, []).

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
 *   constraint_id: ip_category_emergence__first_holding_reading
 *   human_readable: Statutory Author Entry into IP Rights Claimant Set (1710)
 *   domain: legal_philosophy/intellectual_property
 *
 * SUMMARY:
 *   In 1710, the Statute of Anne formally recognized authors as legitimate
 *   claimants to property rights in their published works, ending the
 *   Stationers' de facto perpetual monopoly on book copying. This constraint
 *   story instantiates the 'first holding' reading of the IP category
 *   emergence kernel: it emphasizes the membership shift in the occupied set
 *   of IP claimants. Before 1710, only the Stationers—an organized, chartered
 *   monopoly—could claim copying rights by administrative registration. After
 *   1710, individual authors entered the claimant set as primary statutory
 *   rightholders, displacing the Stationers to a secondary registration role.
 *   The reading does not ask whether 'ownable expression' became thinkable
 *   (that is the 'thinkability reading'); it asks who was admitted to hold,
 *   and how the change in holder identity reshaped the constraint's
 *   extractiveness profile. The kernel is contested: the
 *   'synchronic/diachronic seam' reading questions whether first-holding and
 *   thinkability are formally independent or time-framing artifacts; the
 *   'thinkability reading' emphasizes the ontological emergence of IP as a
 *   category distinct from the occupancy shift.
 *
 * KEY AGENTS:
 *   - statutory_authors: Labor-theory claimants newly recognized as primary holders; moderate power, constrained exit (cannot unclaim authorship)
 *   - stationers_guild: Former monopoly administrator losing primacy to author claims; organized power, constrained exit (must retain registration role but under subordinate authority)
 *   - parliament: Committer frame; institutional power, analytical seat; chose the new claimant set in response to visible monopoly extraction
 *   - competing_publishers: Outside-Guild printers newly able to negotiate with authors but faced with higher transaction costs and legal uncertainty
 *   - reading_public: Powerless diffuse beneficiary (time-limited reversion) and payer (higher prices from rights negotiation costs)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, 0.62).
domain_priors:suppression_score(ip_category_emergence__first_holding_reading, 0.71).
domain_priors:theater_ratio(ip_category_emergence__first_holding_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(ip_category_emergence__first_holding_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__first_holding_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__first_holding_reading, "Statutory Author Entry into IP Rights Claimant Set (1710)").
narrative_ontology:topic_domain(ip_category_emergence__first_holding_reading, "legal_philosophy/intellectual_property").

domain_priors:requires_active_enforcement(ip_category_emergence__first_holding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__first_holding_reading, 'e781f0cc-fbcf-4497-831f-857364a1eb8b').
narrative_ontology:cs_kernel_codification('e781f0cc-fbcf-4497-831f-857364a1eb8b', formalized).
narrative_ontology:cs_authority_grounding('e781f0cc-fbcf-4497-831f-857364a1eb8b', extraction).
narrative_ontology:cs_interpretation_layer_present('e781f0cc-fbcf-4497-831f-857364a1eb8b').
narrative_ontology:cs_reading_relation('e781f0cc-fbcf-4497-831f-857364a1eb8b', ip_category_emergence__thinkability_reading, coexists_with).
narrative_ontology:cs_reading_relation('e781f0cc-fbcf-4497-831f-857364a1eb8b', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('e781f0cc-fbcf-4497-831f-857364a1eb8b', foundational, occupancy_shift_determines_extraction).
narrative_ontology:cs_axiom_status(occupancy_shift_determines_extraction, holdable).
narrative_ontology:cs_axiom_grounding('e781f0cc-fbcf-4497-831f-857364a1eb8b', occupancy_shift_determines_extraction, empirically_contingent).
narrative_ontology:cs_axiom('e781f0cc-fbcf-4497-831f-857364a1eb8b', foundational, labor_theory_legitimacy_of_author_claims).
narrative_ontology:cs_axiom_status(labor_theory_legitimacy_of_author_claims, holdable).
narrative_ontology:cs_axiom_grounding('e781f0cc-fbcf-4497-831f-857364a1eb8b', labor_theory_legitimacy_of_author_claims, deontological).
narrative_ontology:cs_reference_frame('e781f0cc-fbcf-4497-831f-857364a1eb8b', stationers_monopoly_regime).
narrative_ontology:cs_drift_state('e781f0cc-fbcf-4497-831f-857364a1eb8b', statute_of_anne_enactment, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('e781f0cc-fbcf-4497-831f-857364a1eb8b', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(ip_category_emergence__first_holding_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, stationers_guild).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, competing_publishers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__first_holding_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__first_holding_reading, reading_public).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Writers, composers, and creators newly admitted as legitimate claimants to perpetual or extended property rights in their works. Before 1710, they held no statutory standing; after the Statute of Anne, they held a fourteen-year renewable exclusive right to copy. They benefit from the shift because their labor-derived claim is now recognized as legally coherent and enforceable. Exit options are constrained: they cannot unclaim authorship, nor can they return to the pre-1710 regime where only guild-authorized stationers held copying rights.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, statutory_authors, beneficiary,
    moderate, biographical, constrained, national).

% The London Stationers' Company had held a monopoly on book copying under royal charter and practice since the 1550s. They administered copying rights by admitting specific works to their Register, collecting dues, and excluding unauthorized printers. The 1710 statute dismantles their monopoly by introducing author-based claims that supersede the Stationers' administrative authority. They pay through loss of monopoly power and must now compete with authors for property claims. They retain some enforcement role (they administer registration) but their authority is now secondary to author standing.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, stationers_guild, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, stationers_guild, agenda_setter).

% Publishers and printers outside the Stationers' circle had been excluded from the monopoly. The statute theoretically opens access by tying rights to authorship rather than to Stationers' membership. However, they find themselves newly constrained: they can no longer print any work registered by the Stationers (as they could not before), but now must also negotiate with individual authors, whose legal standing is new and whose bargaining position shifts over the interval. They gain access to some works (unregistered or author-initiated) but face higher transaction costs and legal uncertainty in the new regime.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, competing_publishers, payer,
    moderate, biographical, constrained, national).

% Readers benefit from the statute's explicit time limit (fourteen years, then renewable)—works eventually enter common use. They also pay indirectly through higher prices where publisher costs of rights negotiation are passed on. Their options are bounded by which works the new regime permits to circulate; they cannot access pre-1710 works held by Stationers during the copyright term, and they must wait out the new author-based term.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, reading_public, beneficiary,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__first_holding_reading, reading_public, payer).

% Parliament enacted the Statute of Anne in response to the Stationers' monopoly becoming visible and contestable as a constraint. Legislators set the terms: fourteen-year renewable terms, author as the primary claimant, mandatory registration, eventual reversion to common use. Parliament retains power to amend or repeal the statute; their analysis seat is the committer frame—they chose which claimant set would be recognized as legitimate.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, parliament, agenda_setter,
    institutional, generational, analytical, national).

% Unlicensed printers and itinerant reproducers who operated underground or at the margins. The statute does not admit them as legitimate claimants; they remain excluded. The new author-based regime still requires them to negotiate or infringe, and enforcement against them likely intensifies once author rights are codified in statute.
narrative_ontology:constraint_stakeholder(ip_category_emergence__first_holding_reading, common_printers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__first_holding_reading, statutory_authors).
narrative_ontology:fixing_cost_class(ip_category_emergence__first_holding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, codified, time-limited claimant category for intellectual property in published works—the author. Replaces the Stationers' perpetual monopoly (de facto perpetual by renewal and transfer) with a statutory bounded-term regime. Coordinates the recognition problem: who may claim a work, for how long, and what happens when the claim expires.
% TRANSFER_FUNCTION: Moves the authority to grant copying rights from the Stationers' Guild (as monopoly administrator) to individual authors (as statutory primary claimants), with a fourteen-year exclusive term for the author, renewable once. Publishers and printers must now negotiate with authors rather than with the Guild. The transfer is also directional: the public eventually gains access to works when the copyright term expires and works revert to common use.
% ABSENT_VOICES: Unlicensed printers and oral/performance creators (who are not 'authors' under the statute's textual definition) have no seat at the table and remain excluded from statutory standing. Stationers retain a registration-administrative role but lose primacy. The pre-1710 common-law claimants (whoever could seize a manuscript) are not party to the legal shift and would argue against the statute's recognition of abstract labor-derived claims.
% DISAPPEARANCE_RATIONALE: If the statute vanished and Stationers' monopoly re-solidified, publishers would operate under the old charter-monopoly model. The public would lose the time-limited guarantee of reversion to common use. Authors would lose statutory standing but might retain common-law claims (the question is contested historiographically—did authors have pre-1710 common-law rights?). The statute's disappearance would reshape publishing incentives, author bargaining power, and public access, but the contestation arises because the pre-statute regime was itself stable for 150+ years—did it have inherent necessity or did it persist by inertia?
% FOUNDING_PROBLEM: The Stationers' monopoly, while stable, was becoming economically visible as a constraint on publishers outside the Guild and on authors seeking compensation for their labor. Parliament responded to complaints that the monopoly extracted rent without justification tied to the cost of distribution or security. The founding problem was not the absence of book production—that worked—but the visibility of monopoly extraction and the rise of labor-theory arguments for author compensation.
% FOUNDING_PROBLEM_CORROBORATION: Parliament's own legislative record (debates, petitions to Parliament from outside-Guild publishers, grievances from the Stationers about their eroding monopoly) attests the founding problem. However, the Stationers and some political factions attest that the problem was overblown and that the monopoly served quality control and security. Independent historiography (scholars of print history outside the benefiting parties) finds evidence supporting both readings: the monopoly did extract rent, but it also funded quality review and stable production networks. The corroboration is mixed, not from a unified external source.
narrative_ontology:disappearance_verdict(ip_category_emergence__first_holding_reading, contested).
narrative_ontology:founding_problem_status(ip_category_emergence__first_holding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__first_holding_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ip_category_emergence__first_holding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__first_holding_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__first_holding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__first_holding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__first_holding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at 1710, rising from 0.58 in 1660) because the constraint redistributes monopoly extraction from the Stationers to individual authors, reducing the Stationers' rents but not eliminating extraction—publishers and readers still bear costs. Suppression rises over the interval (0.65→0.71) because enforcement of author rights requires new legal machinery (courts, registration systems, prosecution of unauthorized copying) that was less formalized under the Stationers' administrative monopoly. Theater rises (0.35→0.44) because the new regime invokes labor theory and personhood narratives (the author is the 'natural' owner of their creation) that perform ideological work beyond the mechanical enforcement of exclusive rights. The measurement series track the transition: extractiveness dips slightly at 1710 (the redistribution moment) as the Stationers' concentrated rents scatter across many authors, then rebounds as the new author-based system stabilizes. Theater continues rising as the justificatory narrative becomes institutionalized. Resistance (authored in base_properties but not tracked in measurements) is moderate (0.52) because authors themselves support the regime while the Stationers mount legal challenges and outside-Guild publishers face adaptation costs.
 *
 * PERSPECTIVAL GAP:
 *   The statutory-author and Stationers' Guild seats compute radically different constraint types from the same structural data. From the author's seat, the statute is a rope or even a mountain (it vindicates labor theory and recognizes inherent property claims derived from creation). From the Stationers' seat, the statute is a snare (it extracts their monopoly power through legal force and allocates it to an outside claimant set). Parliament's analytical seat sees a tangled_rope: genuine coordination problem (who may claim works, for how long) coupled with asymmetric benefit distribution (authors gain primacy, Stationers lose rents). The engine computes these divergences from the structural data—directional differences in power, exit options, and time horizons—without requiring the authored claim to pre-commit to any one seat's perception.
 *
 * DIRECTIONALITY LOGIC:
 *   Statutory authors are structural beneficiaries (d near 0.2–0.3): they gain property claims they did not possess before, even though the claims are time-limited. The Stationers are targets (d near 0.7–0.8): they lose monopoly power and must now compete with authors for claimant primacy. Parliament, as the institutional committer seat, enforces the new occupancy structure and has high directional independence (d = 0.5, symmetric in power and outcome). Publishers outside the Guild are ambiguously positioned (d near 0.5): they gain access to author-negotiated copying rights but face higher transaction costs and legal uncertainty; their exit options improve (they can now negotiate with individual authors rather than petitioning the Stationers for admission) but their absolute position may worsen (negotiation is costlier than Guild registration). Readers are powerless (d near 0.75): they pay indirectly through higher prices and bear the time-limited restriction, though they eventually access common-use works when copyright terms expire.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through the occupancy shift itself. The founding problem—visible monopoly extraction by the Stationers—is addressed by the statute's explicit redefinition of who may claim. The mandate of the Stationers' monopoly was to provide secure, orderly book production and registration. By 1710, that mandate had partially atrophied: the monopoly was no longer justified as a production necessity but was operating as pure rent collection. Parliament's response was to recognize that the mandate could be served by a different claimant set (individual authors) with bounded terms rather than perpetual guild control. The statute kills the old mandate (Stationers' monopoly as the legitimate coordinating structure) and institutes a new one (author-based time-limited rights plus eventual common use). This is not piton dynamics (the old regime dissolving into theater) but rather a genuine mandatrophy resolution: the constraint's justifying mandate is explicitly replaced, not just degraded.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorial_preexistence_ambiguity,
    'Did authors possess common-law property claims in their manuscripts before the Statute of Anne, and did statutory recognition shift occupancy or merely codify preexisting claims?',
    'Historiographical analysis of manuscript law, court records, and author-publisher disputes in the pre-1710 period. If authors had common-law standing, the statute is recognition; if they did not, it is genuine innovation in the claimant set.',
    'If authors preexisted in common law, the constraint''s mandate did not shift (protection of authorial labor persisted); the statute is a rope (coordination through codification). If authors were novel, the mandate replaced Stationers'' monopoly with author-based claims; the constraint is a tangled_rope (monopoly displacement plus author entry). This omega determines whether the reading is accurate as a first-holding shift or merely a codification event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorial_preexistence_ambiguity, empirical, 'Whether author claims preexisted at common law or statutory recognition created a new claimant category.').

omega_variable(
    coordination_vs_extraction_boundary,
    'Is the Statute of Anne primarily a solution to the coordination problem (how to assign exclusive rights without perpetual litigation) or primarily a displacement of extraction (from Stationers'' monopoly to a broader author-based system)?',
    'Counterfactual institutional analysis: what would happen if Parliament had solved the coordination problem without displacing the Stationers (e.g., by reforming Stationers'' monopoly terms while keeping them as primary claimants)? If coordination could be solved without the occupancy shift, then the statute targets extraction displacement.',
    'If coordination is primary, the statute is a rope (genuine collective-action solution). If extraction displacement is primary, the statute is a tangled_rope (some agents coordinated, others targeted for monopoly displacement). This determines the constraint''s canonical justification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether the statute''s structural change is primarily about solving coordination or displacing monopoly extraction.').

omega_variable(
    theater_ratio_rise_driver,
    'Is the rise in theater_ratio over the interval (0.35→0.48) driven by increasing invocation of labor theory and personhood narratives to justify author rights, or by the growing gap between the statute''s stated time-limited term and its effective perpetualization through transfer and family inheritance?',
    'Textual and legal analysis of judicial opinions, parliamentary debates, and author advocacy materials over the 1710–1750 interval. Track whether rhetoric emphasizes author-as-creator (labor theory, performance of legitimacy) or focuses on the mechanical enforcement of the statutory term.',
    'If labor theory is rising, the theater increase reflects growing ideological justification (more performance, less mechanical enforcement). If perpetualization is rising, the theater increase reflects the statute''s term-limiting mandate being hollowed out in practice (the Goodhart effect—copying right was supposed to be temporary but became de facto perpetual). This determines whether the constraint is being defended through narrative (theater) or through practice drift (mandate atrophy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_ratio_rise_driver, empirical, 'Whether rising theater is driven by labor-theory legitimation or by effective term-perpetualization.').

omega_variable(
    kernel_vs_reading_determination,
    'Is this reading''s claim—that occupancy shift is the determining structural feature—dependent on a particular interpretation of the kernel (the Statute of Anne), or does it follow from the statute''s logical structure regardless of interpretation?',
    'Compare this reading''s occupancy-focus analysis with the ''thinkability_reading'' and ''synchronic_diachronic_seam'' reading. If both analyze the same statutory text but reach different conclusions about what determines the constraint, the kernel itself is under-specified and reading-dependent.',
    'If the occupancy-shift reading is kernel-independent, it is a factual claim about the statute''s structure (first-holding is simply true as written). If it is reading-dependent, occupancy shift is one interpretive frame among several, and sibling readings remain live as alternative frames of the same kernel. This determines whether the reading is a factual discovery or a choice among competing valid framings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_vs_reading_determination, conceptual, 'Whether first-holding is kernel-independent (factual) or reading-dependent (interpretive).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__first_holding_reading, 1660, 1750).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_c_tr_t1660, ip_category_emergence__first_holding_reading, theater_ratio, 1660, 0.35).
narrative_ontology:measurement(ip_c_tr_t1690, ip_category_emergence__first_holding_reading, theater_ratio, 1690, 0.42).
narrative_ontology:measurement(ip_c_tr_t1710, ip_category_emergence__first_holding_reading, theater_ratio, 1710, 0.44).
narrative_ontology:measurement(ip_c_tr_t1725, ip_category_emergence__first_holding_reading, theater_ratio, 1725, 0.46).
narrative_ontology:measurement(ip_c_tr_t1750, ip_category_emergence__first_holding_reading, theater_ratio, 1750, 0.48).

% Extraction over time
narrative_ontology:measurement(ip_c_be_t1660, ip_category_emergence__first_holding_reading, base_extractiveness, 1660, 0.58).
narrative_ontology:measurement(ip_c_be_t1690, ip_category_emergence__first_holding_reading, base_extractiveness, 1690, 0.64).
narrative_ontology:measurement(ip_c_be_t1710, ip_category_emergence__first_holding_reading, base_extractiveness, 1710, 0.62).
narrative_ontology:measurement(ip_c_be_t1725, ip_category_emergence__first_holding_reading, base_extractiveness, 1725, 0.59).
narrative_ontology:measurement(ip_c_be_t1750, ip_category_emergence__first_holding_reading, base_extractiveness, 1750, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ip_c_su_t1660, ip_category_emergence__first_holding_reading, suppression_requirement, 1660, 0.65).
narrative_ontology:measurement(ip_c_su_t1690, ip_category_emergence__first_holding_reading, suppression_requirement, 1690, 0.68).
narrative_ontology:measurement(ip_c_su_t1710, ip_category_emergence__first_holding_reading, suppression_requirement, 1710, 0.71).
narrative_ontology:measurement(ip_c_su_t1725, ip_category_emergence__first_holding_reading, suppression_requirement, 1725, 0.72).
narrative_ontology:measurement(ip_c_su_t1750, ip_category_emergence__first_holding_reading, suppression_requirement, 1750, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__first_holding_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ip_category_emergence__first_holding_reading, 0.12).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__thinkability_reading).
narrative_ontology:affects_constraint(ip_category_emergence__first_holding_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% The 'ip_category_emergence' kernel decomposes into three constraint stories, each instantiating a different reading. The 'first_holding_reading' (this story) emphasizes occupancy shift in the claimant set (Stationers → authors) as the determinant of the constraint's structural change. The 'thinkability_reading' emphasizes the emergence of IP as a conceptually coherent category (ownable expression as a legal fact). The 'synchronic_diachronic_seam' reading questions whether these two framings are formally independent or whether temporal framing conflates them. All three share a common referent (the Statute of Anne and its effects on publishing law) but instantiate different epsilon values and claimant asymmetries. The first_holding_reading influences the other two: if occupancy shift is demonstrably the primary driver of extractiveness and enforcement patterns, it constrains the plausibility of alternative readings that deemphasize holder identity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__first_holding_reading, organized, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
